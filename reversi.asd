(asdf:defsystem "reversi"
  :description "My Reversi"
  :author "tetunoshin"
  :license "MIT"
  :version "0.1"
  :depends-on () ;; 他のシステムが依存していない場合は空
  :pathname "src/"
  :components ((:file "constants") ;; 定数を定義するファイル
               (:file "board")     ;; ボード関連のクラス/メソッド
               (:file "player")))  ;; プレイヤー関連のクラス/メソッド

;; ---- 以下を読み込めばOK ----
;; (require 'asdf)
;; (load "/Users/xuyonglong/git-repository/reversi/reversi.asd")
;; (asdf:load-system "reversi")


