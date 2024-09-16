type PlayerId = PlayerId of int

type PlayerKind = Local | Remote | Spectator

type Player<'input> =
  { kind: PlayerKind
    /// Player's inputs at the given frame number. Delayed inputs can exist in the future.
    /// Any inputs older than `predictionLimit` ago are pruned during `update`.
    inputs: Map<int, 'input>
  }

type Rollbackistan<'state, 'input> =
  { /// The maximum number of frames to roll back. Setting to 0 effectively disables delay-based networking.
    /// 10 is said to be a good number for fighting games running at 60fps, as this provides ~160ms of rollback.
    predictionLimit: int
    /// The time by which to delay inputs. Setting to 0 effectively disables delay-based networking.
    /// 2 appears to be a common value. This incurs ~32ms of latency at 60fps.
    inputDelay: int
    /// The current time.
    realFrameIndex: int
    players: Map<PlayerId, Player<'input>>
    /// All game states upto `predictionLimit` ago.
    history: 'state list
    /// Predict the next input based on the given previous inputs. `List.head` is typically adequate.
    /// May consider more inputs in the future, e.g. with state history.
    /// This would allow for potentially more interesting predictions, e.g. that a player will move towards a powerup.
    /// However, this is said not to yield significantly better results as known.
    predict: 'input list -> 'input
    /// Advance the simulation given the current state and the predicted and actual inputs of all registered players.
    /// This function may be called upto a number of times given by `predictionLimit`.
    advance: 'state -> Map<PlayerId, 'input> -> 'state
  }
  
  member this.getFrame index =
    List.tryItem (this.realFrameIndex - index)
    
type UpdateResult<'state, 'input> =
  | AwaitingPlayers of PlayerId list
  | Advanced of Rollbackistan<'state, 'input> * 'state

  static member make predictionLimit inputDelay predict advance =
    { predictionLimit = predictionLimit
      inputDelay = inputDelay
      realFrameIndex = 0
      players = Map.empty
      history = []
      predict = predict
      advance = advance
    }
    
let registerPlayer rollback playerId kind initInput =
  if rollback.players.ContainsKey playerId then
    failwith $"{playerId} already exists"
  else
    rollback.players.Add(playerId, { kind = kind; inputs = Map [ rollback.realFrameIndex, initInput ] })
    
let applyInput time rollback playerId input =
  { rollback with
      players = rollback.players.Change(playerId, Option.map (fun player ->
        { player with inputs = player.inputs.Add(time, input) }))
  }
let applyLocalInputs rollback (inputs: Map<PlayerId, 'input>) =
  let inputTime = rollback.inputDelay + rollback.realFrameIndex
  
  // add inputs to rollback player state and truncate
  let rollback =
    (rollback, inputs)
    ||> Map.fold (applyInput inputTime)
    
  rollback, inputTime
 
/// Assumptions:
/// - no inputs have been lost in transit (packets must be in order)
/// - network inputs are sanitized to exclude local inputs (to prevent cheating)
/// - players are already added
let update (rollback: Rollbackistan<'state, 'input>)
           (networkInputs: Map<int * PlayerId, 'input>)
           : UpdateResult<'state, 'input> =
  let earliestInputtedFrame, earliestInput = Map.minKeyValue networkInputs
  
  match Map.tryFindKey (fun (_, pid) _ -> not (rollback.players.ContainsKey pid)) networkInputs with
  | Some (_, pid) -> failwith $"Received input for unregistered player {pid}"
  | _ -> ()
  
  // integrate inputs from network
  let rollback =
    (rollback, networkInputs)
    ||> Map.fold (fun rollback (time, playerId) input -> applyInput time rollback playerId input)
    
  // prune expired inputs
  let rollback =
    let expireBefore = rollback.realFrameIndex - rollback.predictionLimit
    let players =
      rollback.players
      |> Map.map (fun _ player ->
          { player with
              inputs = player.inputs |> Map.filter (fun time _ -> time >= expireBefore ) })
    { rollback with players = players }
  
  // pause if prediction limit exceeded for any non-spectator player
  let stalledPlayers =
    rollback.players
    |> Map.filter (fun _ p -> p.inputs.IsEmpty)
    
  if not stalledPlayers.IsEmpty then
    AwaitingPlayers (List.ofSeq stalledPlayers.Keys)
    
  else
      
    // fold best inputs on game state from last updated input until realFrameIndex (+1?)
    
    
    let rollback =
      { rollback with history = List.truncate rollback.realFrameIndex rollback.history }
    
    Advanced (rollback, rollback.history.Head)
    // TODO: how to return commands in MMCC-friendly fashion?