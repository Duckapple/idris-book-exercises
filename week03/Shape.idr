module Shape 

export
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
              Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

export
triangle : Double -> Double -> Shape
triangle x y = Triangle x y

export
rectangle : Double -> Double -> Shape
rectangle x y = Rectangle x y

export
circle : Double -> Shape
circle x = Circle x

public export
data ShapeView : Shape -> Type where
  STriangle : ShapeView (triangle x y)
  SRectangle : ShapeView (rectangle x y)
  SCircle : ShapeView (circle x)

export
shapeView : (s : Shape) -> ShapeView(s)
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle
