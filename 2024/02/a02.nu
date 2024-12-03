def a02 [s] {
    let x = open $"($s)02.txt" | split row "\n" | each { |r| split row ' ' | each { |v| $v | into int } }
    $x | filter { |r|
        let is = 0..(($r | length) - 2) | each {||}
        let increasing = $is | all { |i|
            ($r | get $i) <= ($r | get ($i + 1))
        }
        let decreasing = $is | all { |i|
            ($r | get $i) >= ($r | get ($i + 1))
        }
        let differ = $is | all { |i|
          let v = (($r | get $i) - ($r | get ($i + 1))) | math abs
          ($v >= 1) and ($v <= 3)
        }
        print "..."
        print $r
        print $"increasing? ($increasing)"
        print $"decreasing? ($decreasing)"
        print $"differ? ($differ)"
        let safe = ($increasing or $decreasing) and $differ
        print $"safe? ($safe)"
        $safe
    } | length
}
