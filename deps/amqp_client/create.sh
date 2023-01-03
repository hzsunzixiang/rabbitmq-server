
DIR=`pwd`
find -L $DIR/src $DIR/test -name "*.erl" -o -name "*.hrl" > $DIR/cscope_source.files
cscope -bq -i $DIR/cscope_source.files  -f cscope_source.out


#ctags -R *.erl *.hrl

FILE="$DIR/src $DIR/test"

for i in $FILE
do
	cp .vimrc $i
done

