alexandriatunnel:
	lsof -twni tcp:22045 | xargs kill -9
	ssh -fNL 22045:alexandria.marathon.mesos:22045 -p 2200 kirito@asunad-master.southcentralus.cloudapp.azure.com

vulgatetunnel:
	lsof -twni tcp:6205 | xargs kill -9
	ssh -fNL 6205:vulgate.marathon.mesos:6205 -p 2200 kirito@asunad-master.southcentralus.cloudapp.azure.com

profiletunnel:
	lsof -twni tcp:31312 | xargs kill -9
	ssh -fNL 31312:lucinda.marathon.mesos:31312 -p 2200 kirito@asunad-master.southcentralus.cloudapp.azure.com

# TODO(igm): find a cleaner way of doing this
tunnel: alexandriatunnel vulgatetunnel

run:
	sbt "run --alexandria_port=22045 --vulgate_port=6205"
