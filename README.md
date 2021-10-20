# [suscript](https://esolangs.org/wiki/Suscript)

*sussy amogus scripting language*

---

a project made by me and a couple of my friends online in an attempt to make something like [Among Us](https://esolangs.org/wiki/Among_Us), but compiled.

## compiling

pretty straightfoward, just compile `suscript.bas` using [QB64](https://qb64.org).

## usage

see `./suscript --help`

## language

| command | description |
| ------- | ----------- |
| `eject "<text>"` | prints the text |
| `when the impostor is sus 😳:` | begins an if block. if the current town is "sus", then it executes the code. otherwise, it skips to the corresponding `among drip` |
| `among drip` | see above |
| `impostor {sus\|safe}` | sets the current town to sus or safe, respectively |
| `amogus goes to the {next\|previous} sus town` | increments or decrements the town pointer, respectively |
| `say <label>` | jump to a label\* |
| `become the <label>` | defines a label\* |
| `is impostor sus?` | prompts the user, if the user types "sus" then it sets the current town to sus, if the user types "safe" then sets the current town to safe, other inputs are UB |

\*: labels must end in `-us` (ex. `amogus` or `sus`)
