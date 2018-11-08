
cd tests
test_path="../src/"
cmd_init="erl -noshell -pa ../ebin -run symgen main "
cmd_end=" -s init stop"
for test in biggest_ok delete_bug sorted_ok stack mytrees; do
     $cmd_init$test$cmd_end # > $test_path$test".pl"
done
