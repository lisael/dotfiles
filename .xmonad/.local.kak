def -allow-override used_keys %{
    edit -scratch *used_keys*
    exec "ggGEd|.local/used_keys.sh<ret>"
}
