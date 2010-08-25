all: 
	mkdir -p ebin
	erl -noshell -eval 'case make:all() of up_to_date -> halt(0); _ -> halt(1) end'

clean:
	rm -f ebin/*

shell: all
	erl -pa ebin
