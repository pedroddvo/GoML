# GoML
**G**o's **O**wn **ML**  
An ML transpiler which targets Golang, and attempts to be fully compatible with Go.

## Examples
Hello world
```ml
package main
open "fmt"
open "os"

val main : ()
let main =
    let args' = List.map (func x -> fmt.Sprintf "%s, " x) os.Args in
    fmt.Printf "Hello world:\n%v" args'
```
transpiles to
```go
package main

import "fmt"
import "os"

func main() {
    var argsp []string = os.Args
    // map implementation not decided yet
    fmt.Printf "Hello world:\n%v" argsp
}
```

## Thoughts on transpilation
### Currying
Currying in Go is a challenge. Below, is a automatically curried function `add3` written in haskell and go side by side:
```hs
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

add3 1 2 3
```
```go
func add3(x int) func(int) func(int) int {
    return func(y int) func(int) int {
        return func(z int) int {
            return x + y + z
        }
    }
}

add3(1)(2)(3)
```
As such, the transpilation process may have to contextualize currying, and detect symbols at which currying happens, in order to optimize the function `add3` down into a faster and more readable version. Below, is a case at which at no point in the transpilation process has `add3` been detected to curry, and such has been simplified into an uncurried function:
```go
func add3(x, y, z int) int { return x + y + z }
```