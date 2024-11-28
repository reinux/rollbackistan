type PlayerId = PlayerId of int

type Player<'input> =
  { /// Player's inputs at the given frame number. Inputs can exist in the future, in which case they're delayed.
    /// Any inputs older than `maxHistory` ago are pruned during `update`.
    inputs: Map<int, 'input> }
  member player.LastInput =
    if player.inputs.IsEmpty then None
    else
      Some (Map.maxKeyValue player.inputs)
  member player.LastInputAsOf time =
    // TODO: https://github.com/fsharp/fslang-design/pull/734
    Map.toSeq player.inputs
    |> Seq.takeWhile (fun (id, _) -> id <= time)
    |> Seq.rev
    |> Seq.tryHead

type Rollbackistan<'state, 'input> =
  { /// The maximum number of frames to roll back. Setting to 0 effectively disables rollback networking.
    /// 10 is said to be a good number for fighting games running at 60fps, as this provides ~160ms of rollback.
    maxHistory: int
    /// The current time.
    realFrameIndex: int
    players: Map<PlayerId, Player<'input>>
    /// All game states upto `maxHistory` ago.
    history: 'state list
  }

type UpdateResult<'state, 'input> =
  | AwaitingPlayers of Rollbackistan<'state, 'input> * PlayerId list
  | Advanced of Rollbackistan<'state, 'input>

let make<'state, 'input> maxHistory init : Rollbackistan<'state, 'input> =
  { realFrameIndex = 0
    players = Map.empty
    maxHistory = maxHistory
    history = [ init ] }

/// Assumptions:
/// - no inputs have been lost in transit (packets must be in order)
/// - network inputs are sanitized to exclude local inputs (to prevent cheating)
let update predict
           advance
           (networkInputs: Map<int * PlayerId, 'input>)
           (rollback: Rollbackistan<'state, 'input>)
           : UpdateResult<'state, 'input> =
             
  let rollback = { rollback with realFrameIndex = rollback.realFrameIndex + 1}
  
  let expireBefore = rollback.realFrameIndex - rollback.maxHistory

  // integrate inputs from network
  let rollback =
    (rollback, networkInputs)
    ||> Map.fold (fun rollback (time, playerId) input ->
          { rollback with
              players = rollback.players.Change(playerId, function
                | Some player -> Some { player with inputs = player.inputs.Add(time, input) }
                | None -> Some { inputs = Map [ time, input ] }) })

  // prune expired inputs
  let rollback =
    let players =
      rollback.players
      |> Map.map (fun _ player ->
          let prunedInputs = player.inputs |> Map.filter (fun time _ -> time >= expireBefore )
          let prunedInputs =
            if prunedInputs.IsEmpty then Map [ Map.maxKeyValue player.inputs ]
            else prunedInputs
          { player with inputs = prunedInputs })
    { rollback with players = players }

  // pause if prediction limit exceeded for any non-spectator player
  let stalledPlayers =
    rollback.players
    |> Map.filter (fun _ p -> match p.LastInput with None -> false | Some (time, _) -> time < expireBefore)

  if not stalledPlayers.IsEmpty then
    AwaitingPlayers ({ rollback with realFrameIndex = rollback.realFrameIndex - 1 }, List.ofSeq stalledPlayers.Keys)

  else
    let startTime =
      networkInputs.Keys
      |> Seq.tryHead
      |> Option.map fst
      |> Option.defaultValue rollback.realFrameIndex
    
    // another player may not have sent inputs in this update, so predict those inputs up to starting frame
    let startingInputs =
      seq {
        for p in rollback.players do
          let playerId = p.Key
          let player = p.Value
          let inputTime, input =
            match player.LastInputAsOf startTime with
            | None -> failwith $"{playerId} has no input as of frame {startTime}"
            | Some (inputTime, input) ->
              inputTime, input
          let input =
            let rec predictLoop time input =
              if time < startTime then
                predictLoop (time + 1) (predict [ input ])
              else
                input
            predictLoop inputTime input
          playerId, input }
      
    let startState =
      List.item (max 0 (rollback.realFrameIndex - startTime)) rollback.history
      
    // play back the game state from the earliest of the new inputs until the next real-time frame
    let playback =
      List.unfold (fun (state, inputs, time) ->
        if time <= rollback.realFrameIndex then
          let inputs =
            inputs
            |> Seq.map (fun (playerId, input) ->
                match Map.tryFind time rollback.players[playerId].inputs with
                | None ->
                  playerId, predict [ input ]
                | Some input ->
                  playerId, input
              )
          let state = advance state (Map inputs)
          Some (state, (state, inputs, time + 1))
        else None)
        
    // update history with new played back state
    let history =
      playback (startState, startingInputs, startTime)
      |> fun newHistory ->
          (List.rev newHistory)@(List.skip (newHistory.Length - 1) rollback.history)
          |> List.truncate rollback.maxHistory
    
    let rollback =
      { rollback with
          history = history }
      
    // TODO: how to return commands in MMCC-friendly fashion?
    
    Advanced rollback
 
type State =
  { stateTime: int
    player1Pressed: bool }
 
type Input =
  { buttonPressed: bool }

let advance state (inputs: Map<PlayerId, Input>) =
  let p1Pressed =
    match Map.tryFind (PlayerId 1) inputs with
    | None -> false
    | Some input -> input.buttonPressed
  { state with stateTime = state.stateTime + 1; player1Pressed = p1Pressed }

let init = { stateTime = 1000; player1Pressed = false }

let rollback =
  make<State, Input> 5 init
  
let mapResult f = function
  | Advanced rollback -> f rollback
  | AwaitingPlayers (rollback, _) -> f rollback
let (|%>) x y = x |> mapResult (update List.head advance y)

// TODO: figure out if this works even if inputs come in in the wrong order (it should)
// TODO: don't roll back if inputs are same as predicted
// TODO: advance function should take more rollback state, e.g. current frame, whether it's a playback etc.
// TODO: disconnect function
// TODO: use generic PlayerId

rollback
|> update List.head advance (Map [ ]) 
|%> Map [ (1, PlayerId 1), { buttonPressed = true } ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ (7, PlayerId 1), { buttonPressed = false } ]
// |%> Map [ (6, PlayerId 1), { buttonPressed = false } ]
|%> Map [ ]
|%> Map [ ]
|%> Map [ ]

(*

Legend
-  No input
*  Player joined
%  Player disconnected
X  Button pressed state
x  Button released state
S  Stalled (until resumed)
       S
  01234567890123456789
1 *----XXXXXxxxxxxxxx
2     *--XXXX

*)