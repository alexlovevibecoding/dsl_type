type Point = { x: Int; y: Int };
type Shape =
    | Circle { radius: Float }
    | Rect   { width: Float; height: Float }
    | Unknown;
type Option<T> =
    | None
    | Some(T);

type Tree<T> =
    rec Self.
        | Empty
        | Node { value: T; left: Self; right: Self };

type Tree<T> =
    rec Self.
        { value: T; children: List<Self> };

type List<T> =
    rec Self.
        | Nil
        | Cons { head: T; tail: Self };

type List<T> =
    rec Self.
        | Nil
        | Cons { head: T; tail: Self };