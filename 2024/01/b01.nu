def b01 [s] {
    let x = open $"($s)01.txt" | split row "\n" | split column -r '\s+' a b
    let a = $x | get a | into int | sort
    let b = $x | get b | into int | sort
    0..(($a | length) - 1) | each { |i|
        let a = $a | get $i | into int
        let bs = $b | filter { |b| $b == $a } | length
        $a * $bs
    } | math sum
}
