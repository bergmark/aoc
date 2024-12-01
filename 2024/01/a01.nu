def a01 [s] {
    let x = open $"($s)01.txt" | split row "\n" | split column -r '\s+' a b
    let a = $x | get a | into int | sort
    let b = $x | get b | into int | sort
    0..(($a | length) - 1) | each { |i|
        (($a | get $i | into int) - ($b | get $i | into int)) | math abs
    } | math sum
}
