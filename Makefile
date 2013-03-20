all:
	mvn install
	mvn assembly:single

choco:
	git clone https://github.com/JLiangWaterloo/choco2 choco2
	make -C choco2

clean:
	mvn clean
	rm -rf choco2
