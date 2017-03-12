alexandriatunnel:
	lsof -twni tcp:22045 | xargs kill -9
	ssh -fNL 22045:ecs-1.muramasa.dev:22045 ec2-user@bastion.dev.asuna.io

vulgatetunnel:
	lsof -twni tcp:6205 | xargs kill -9
	ssh -fNL 6205:ecs-0.muramasa.dev:6205 ec2-user@bastion.dev.asuna.io

# TODO(igm): find a cleaner way of doing this
tunnel: alexandriatunnel vulgatetunnel

run:
	sbt "run --alexandria_port=22045 --vulgate_port=6205"
