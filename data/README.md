# このディレクトリの役割

`data`はあなたが分析で用いるデータを入れておくためのディレクトリ(フォルダ)です。分析用のソースコードなどと一緒にすると訳がわからなくなるので、ディレクトリを分けましょう。**分析や処理の再現性**を確保するために重要な技術の一つです。


___

## サブディレクトリの構成 

`data`ディレクトリの中身は次のサブディレクトリで構成するのが良いでしょう。

- `in`：rawデータを入れておく場所。名前は`in`とか`raw`とかつけます。特別な事情がない限り、**このディレクトリにあるファイルの中身を(直接、手動で)書き換えてはいけません。**

- `out`:加工したデータを入れておく場所。名前は`out`とか`processed`とかつけます。Rなどで自分の分析に合うように加工したデータはrawデータと分けてこちらに入れておきましょう。つまり`rawデータ`→  `Rなどで加工` → `out`に吐き出す、という流れです。
