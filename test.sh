
cd tests
test_path="../src/"
cmd_init="erl -noshell -pa ../ebin -run symgen main "
cmd_end=" -s init stop"
for test in biggest_ok; do
     $cmd_init$test$cmd_end # > $test_path$test".pl"
done
