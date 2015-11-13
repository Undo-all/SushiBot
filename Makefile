all: sushiBot

sushiBot: Main.hs Bot.hs 
	ghc Main.hs Bot.hs -o sushiBot -O2

.PHONY: clean
clean:
	rm *.hi *.o

.PHONY: run
run: sushiBot
	nohup ./sushiBot > /dev/null 2> /dev/null &

.PHONY: restart
restart: sushiBot
	killall sushiBot
	nohup ./sushiBot > /dev/null 2> /dev/null &

