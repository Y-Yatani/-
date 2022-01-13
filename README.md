## 野球における期待得点・得点確率を算出するサンプルコード
打席結果を6通りに限定したうえでそれらに対応する走塁規則を定めると、野球の1イニングにおける期待得点と得点確率を連立方程式の解として求めることができます。

「理論値導出サンプルコード」は打順を構成する各打者の成績(各打席結果をもたらす確率)から、各状況(打順・アウトカウント・塁状況)における期待得点と得点確率を導出するコードです。
