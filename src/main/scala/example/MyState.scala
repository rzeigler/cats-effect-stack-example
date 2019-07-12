package example

final case class MyState[S, A](runState: S => (S, A)) extends AnyVal {
    def map[B](f: A => B): MyState[S, B] = 
        MyState(s => {
            val (s2, a) = runState(s)
            s2 -> f(a)
        })
    
    def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = 
        MyState(s => {
            val (s2, a) = runState(s)
            f(a).runState(s2)
        })

    def run(s: S): A = runState(s)._2
}

object MyState {
    def point[S, A](a: A): MyState[S, A] = MyState(s => (s, a))

    def get[S]: MyState[S, S] = MyState(s => (s, s))

    def set[S](s2: S): MyState[S, Unit] = MyState(s => (s2, ()))

    def inspect[S, A](f: S => A): MyState[S, A] = MyState(s => (s, f(s)))

    def modify[S](f: S => S): MyState[S, S] = MyState(s => {
        val s2 = f(s)
        (s2, s2)
    })
}

