module RibbitChat

open Godot
[<Measure>] type godotLengthUnit // pixel or something
type godotLength = float32<godotLengthUnit>

type Circle = { origin: Vector2; radius: godotLength; color: Color }
type Rect = { origin: Vector2; height: godotLength; width: godotLength; color: Color }
type Shape =
    | Circle of Circle
    | Rect of Rect
type Command = 
    | Draw of Shape


let godotLengthInverse = 1.0f/1.0f<godotLengthUnit>

type ShapePanel() =
    inherit Panel()
    member val Shapes = [] with get, set
    override self._Draw() =
        for x in self.Shapes do
            match x with
            | Circle c -> 
                self.DrawCircle(c.origin, c.radius * godotLengthInverse, c.color)
            | Rect r -> 
                let rect = Rect2(r.origin, r.height * godotLengthInverse, r.width * godotLengthInverse)
                self.DrawRect(rect, r.color, true, 4f)
    member self.AddShape shape =
        self.Shapes <- shape::self.Shapes
        self.Update()

let lookup<'t when 't :> Node> (pathString: string) (node: Node) =
    use path = new NodePath(pathString)
    match node.GetNode(path) with
    | :? 't as child -> Some child
    | _ -> None

let lookupAction<'t when 't :> Node> (node: Node) (pathString: string)  f =
    use path = new NodePath(pathString)
    match node.GetNode(path) with
    | :? 't as child -> f child
    | _ -> ()

let ifPresent f = function
    | Some v -> f v
    | None -> ()

let rand = new Godot.RandomNumberGenerator()
rand.Randomize()
open Packrat

let mutable history = []
let mutable historyDepth = 0

type ChatWindow() =
    inherit Control()
    override self._Ready() =
        lookupAction<Control> self "UserInput" <| fun userInput -> userInput.GrabFocus()
    override self._Input(event) =
        match event with
        | :? InputEventKey as e when e.IsPressed() && e.Scancode = uint32 KeyList.Up ->
            historyDepth <- min (historyDepth + 1) (history.Length)
            lookupAction<LineEdit> self "UserInput" <| fun userInput ->
                userInput.Text <- (""::history).[historyDepth]
                userInput.CaretPosition <- userInput.Text.Length
            self.GetTree().SetInputAsHandled()
        | :? InputEventKey as e when e.IsPressed() && e.Scancode = uint32 KeyList.Down ->
            historyDepth <- max (historyDepth - 1) 0
            lookupAction<LineEdit> self "UserInput" <| fun userInput ->
                userInput.Text <- (""::history).[historyDepth]
                userInput.CaretPosition <- userInput.Text.Length
            self.GetTree().SetInputAsHandled()
        | :? InputEventKey as e when e.IsPressed() && e.Scancode = uint32 KeyList.Enter ->
            lookupAction<LineEdit> self "UserInput" <| fun userInput ->
                if userInput.Text.Trim().Length > 0  then
                    let txt = userInput.Text
                    // add to recent stack if it's not a duplicate
                    match history with
                    | recent::_ when recent = txt -> () // don't add duplicate history entry
                    | _ ->
                        history <- txt::history
                    historyDepth <- 0
                    userInput.Text <- ""
                    let (|OptionalColor|_|) = function
                        | Word(color, End) -> Some (Some color)
                        | End -> Some None
                        | _ -> None
                    
                    let (|CircleArgs|_|) = function
                        | Str "circle" (Int (x, Int(y, Int(radius, (OptionalColor color))))) -> Some(Some(single x), Some(single y), Some (single radius), color)
                        | Str "circle" (Int (x, Int(y, (OptionalColor color)))) -> Some(Some(single x), Some(single y), None, color)
                        | Str "circle" (Int (radius, (OptionalColor color))) -> Some(None, None, Some(single radius), color)
                        | Str "circle" (OptionalColor color) -> Some(None, None, None, color)
                        | _ -> None

                    let circle (x,y,radius,color) = function
                        | Some (hud: ShapePanel) ->
                            let sz = hud.RectSize
                            hud.AddShape(Circle { origin = Vector2(defaultArg x (rand.Randf() * sz.x), defaultArg y (rand.Randf() * sz.y)); radius = 1f<godotLengthUnit> * defaultArg radius (rand.Randf() * 50f); color = Color.ColorN(defaultArg color "blue") })
                        | None -> ()
                    match txt |> ParseArgs.Init with
                    | CircleArgs(x,y,radius,color) -> self |> lookup<ShapePanel> "HUD" |> circle(x,y,radius,color)
                    | _ ->
                        lookupAction<TextEdit> self "ChatDisplay" <| fun chatDisplay ->
                            chatDisplay.Text <- chatDisplay.Text + txt + "\n"
        | _ ->
            ()