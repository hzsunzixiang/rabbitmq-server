
DIR=`pwd`
find -L $DIR/amqp_client/src $DIR/amqp_client/include  $DIR/rabbit_common/src  $DIR/rabbit_common/include -name "*.erl" -o -name "*.hrl" > $DIR/cscope_source.files
cscope -bq -i $DIR/cscope_source.files  -f cscope_source.out


#ctags -R *.erl *.hrl

FILE="$DIR/amqp_client/src $DIR/amqp_client/include  $DIR/rabbit_common/src  $DIR/rabbit_common/include"

for i in $FILE
do
	cp .vimrc $i
done

