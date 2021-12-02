extends Control


# Declare member variables here. Examples:
# var a = 2
# var b = "text"


# Called when the node enters the scene tree for the first time.
func _ready():
	pass

func _input(event):
	if event is InputEventKey:
		if event.pressed and event.scancode == KEY_ENTER:
			var cmd = $UserInput.text
			$UserInput.text = ""
			execute(cmd)

func chat(txt):
	$ChatDisplay.text += txt + "\n"

func execute(txt):
	if txt == "circle":
		var sz = $HUD.rect_size
		$HUD.draw_circle(Vector2(randf()*sz.x, randf()*sz.y), 20, Color.blue)
		$HUD.update()
	else:
		chat(txt)
