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

//let (|Path|_|)<'t when 't :> Node> (path:string) (self : Node) =
//    match self.GetNode(path: string) with
//    | :? 't as child -> Some child
//    | _ -> None
let lookup<'t when 't :> Node> (pathString: string) (node: Node) =
    use path = new NodePath(pathString)
    match node.GetNode(path) with
    | :? 't as child -> Some child
    | _ -> None

let ifPresent f = function
    | Some v -> f v
    | None -> ()

let rand = new Godot.RandomNumberGenerator()
rand.Randomize()
open Packrat

type ChatWindow() =
    inherit Control()
    override self._Input(event) =
        match event with
        | :? InputEventKey as e when e.IsPressed() && e.Scancode = uint32 KeyList.Enter ->
            match self |> lookup<LineEdit> "UserInput" with
            | Some userInput ->
                let txt = userInput.Text
                userInput.Text <- ""
                let (|OptionalColor|_|) = function
                    | Word(color, End) -> Some (Some color)
                    | End -> Some None
                    | _ -> None
                    
                let (|CircleArgs|_|) = function
                    | Str "circle" (OptionalColor color) -> Some(None, None, None, color)
                    | Str "circle" (Int (radius, (OptionalColor color))) -> Some(None, None, Some(single radius), color)
                    | Str "circle" (Int (x, Int(y, (OptionalColor color)))) -> Some(Some(single x), Some(single y), None, color)
                    | Str "circle" (Int (x, Int(y, Int(radius, (OptionalColor color))))) -> Some(Some(single x), Some(single y), None, color)
                    | _ -> None

                let circle (x,y,radius,color) = function
                    | Some (hud: ShapePanel) ->
                        let sz = hud.RectSize
                        hud.AddShape(Circle { origin = Vector2(defaultArg x (rand.Randf() * sz.x), defaultArg y (rand.Randf() * sz.y)); radius = 1f<godotLengthUnit> * defaultArg radius (rand.Randf() * 50f); color = Color.ColorN(defaultArg color "blue") })
                    | None -> ()
                match txt |> ParseArgs.Init with 
                | CircleArgs(x,y,radius,color) -> self |> lookup<ShapePanel> "HUD" |> circle(x,y,radius,color)
                | _ ->
                    (self |> lookup<TextEdit> "ChatDisplay") |> ifPresent (fun chatDisplay ->
                        chatDisplay.Text <- chatDisplay.Text + txt + "\n")
            | None -> ()
        | _ -> ()