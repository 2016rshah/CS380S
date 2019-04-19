[ CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 1) ( "test.c" : line 1 , 4 ) Name { nameId = 1 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "t"
                      116
                      (NodeInfo
                         ("test.c" : line 1)
                         ( "test.c" : line 1 , 1 )
                         Name { nameId = 0 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 2 }))
         , Nothing
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 3 }))
, CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 2) ( "test.c" : line 2 , 4 ) Name { nameId = 5 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "f"
                      102
                      (NodeInfo
                         ("test.c" : line 2)
                         ( "test.c" : line 2 , 1 )
                         Name { nameId = 4 })))
                [ CFunDeclr
                    (Right
                       ( [ CDecl
                             [ CTypeSpec
                                 (CCharType
                                    (NodeInfo
                                       ("test.c" : line 2)
                                       ( "test.c" : line 2 , 4 )
                                       Name { nameId = 8 }))
                             ]
                             [ ( Just
                                   (CDeclr
                                      (Just
                                         (Ident
                                            "q"
                                            113
                                            (NodeInfo
                                               ("test.c" : line 2)
                                               ( "test.c" : line 2 , 1 )
                                               Name { nameId = 7 })))
                                      []
                                      Nothing
                                      []
                                      (NodeInfo
                                         ("test.c" : line 2)
                                         ( "test.c" : line 2 , 1 )
                                         Name { nameId = 9 }))
                               , Nothing
                               , Nothing
                               )
                             ]
                             (NodeInfo
                                ("test.c" : line 2) ( "test.c" : line 2 , 1 ) Name { nameId = 10 })
                         , CDecl
                             [ CTypeSpec
                                 (CCharType
                                    (NodeInfo
                                       ("test.c" : line 2)
                                       ( "test.c" : line 2 , 4 )
                                       Name { nameId = 12 }))
                             ]
                             [ ( Just
                                   (CDeclr
                                      (Just
                                         (Ident
                                            "l"
                                            108
                                            (NodeInfo
                                               ("test.c" : line 2)
                                               ( "test.c" : line 2 , 1 )
                                               Name { nameId = 11 })))
                                      []
                                      Nothing
                                      []
                                      (NodeInfo
                                         ("test.c" : line 2)
                                         ( "test.c" : line 2 , 1 )
                                         Name { nameId = 13 }))
                               , Nothing
                               , Nothing
                               )
                             ]
                             (NodeInfo
                                ("test.c" : line 2) ( "test.c" : line 2 , 1 ) Name { nameId = 14 })
                         ]
                       , False
                       ))
                    []
                    (NodeInfo
                       ("test.c" : line 2) ( "test.c" : line 2 , 1 ) Name { nameId = 15 })
                ]
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 2) ( "test.c" : line 2 , 1 ) Name { nameId = 6 }))
         , Nothing
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 2)
          ( "test.c" : line 2 , 1 )
          Name { nameId = 16 }))
, CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 3)
                 ( "test.c" : line 3 , 4 )
                 Name { nameId = 18 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "y"
                      121
                      (NodeInfo
                         ("test.c" : line 3)
                         ( "test.c" : line 3 , 1 )
                         Name { nameId = 17 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 3)
                   ( "test.c" : line 3 , 1 )
                   Name { nameId = 19 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 3)
                         ( "test.c" : line 3 , 3 )
                         Name { nameId = 20 })))
                (NodeInfo
                   ("test.c" : line 3)
                   ( "test.c" : line 3 , 3 )
                   Name { nameId = 21 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 3)
          ( "test.c" : line 3 , 1 )
          Name { nameId = 22 }))
, CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 4)
                 ( "test.c" : line 4 , 4 )
                 Name { nameId = 24 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "z"
                      122
                      (NodeInfo
                         ("test.c" : line 4)
                         ( "test.c" : line 4 , 1 )
                         Name { nameId = 23 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 4)
                   ( "test.c" : line 4 , 1 )
                   Name { nameId = 25 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 4)
                         ( "test.c" : line 4 , 3 )
                         Name { nameId = 26 })))
                (NodeInfo
                   ("test.c" : line 4)
                   ( "test.c" : line 4 , 3 )
                   Name { nameId = 27 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 4)
          ( "test.c" : line 4 , 1 )
          Name { nameId = 28 }))
, CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 5)
                 ( "test.c" : line 5 , 4 )
                 Name { nameId = 30 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "r"
                      114
                      (NodeInfo
                         ("test.c" : line 5)
                         ( "test.c" : line 5 , 1 )
                         Name { nameId = 29 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 5)
                   ( "test.c" : line 5 , 1 )
                   Name { nameId = 31 }))
         , Just
             (CInitExpr
                (CCall
                   (CVar
                      (Ident
                         "f"
                         102
                         (NodeInfo
                            ("test.c" : line 5)
                            ( "test.c" : line 5 , 1 )
                            Name { nameId = 32 }))
                      (NodeInfo
                         ("test.c" : line 5)
                         ( "test.c" : line 5 , 1 )
                         Name { nameId = 33 }))
                   [ CVar
                       (Ident
                          "y"
                          121
                          (NodeInfo
                             ("test.c" : line 5)
                             ( "test.c" : line 5 , 1 )
                             Name { nameId = 34 }))
                       (NodeInfo
                          ("test.c" : line 5) ( "test.c" : line 5 , 1 ) Name { nameId = 35 })
                   , CVar
                       (Ident
                          "z"
                          122
                          (NodeInfo
                             ("test.c" : line 5)
                             ( "test.c" : line 5 , 1 )
                             Name { nameId = 36 }))
                       (NodeInfo
                          ("test.c" : line 5) ( "test.c" : line 5 , 1 ) Name { nameId = 37 })
                   ]
                   (NodeInfo
                      ("test.c" : line 5)
                      ( "test.c" : line 5 , 1 )
                      Name { nameId = 38 }))
                (NodeInfo
                   ("test.c" : line 5)
                   ( "test.c" : line 5 , 1 )
                   Name { nameId = 39 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 5)
          ( "test.c" : line 5 , 1 )
          Name { nameId = 40 }))
, CDeclExt
    (CDecl
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 6)
                 ( "test.c" : line 6 , 4 )
                 Name { nameId = 42 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "k"
                      107
                      (NodeInfo
                         ("test.c" : line 6)
                         ( "test.c" : line 6 , 1 )
                         Name { nameId = 41 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 6)
                   ( "test.c" : line 6 , 1 )
                   Name { nameId = 43 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 6)
                         ( "test.c" : line 6 , 3 )
                         Name { nameId = 44 })))
                (NodeInfo
                   ("test.c" : line 6)
                   ( "test.c" : line 6 , 3 )
                   Name { nameId = 45 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 6)
          ( "test.c" : line 6 , 1 )
          Name { nameId = 46 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test.c" : line 7)
                 ( "test.c" : line 7 , 3 )
                 Name { nameId = 48 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test.c" : line 7)
                   ( "test.c" : line 7 , 4 )
                   Name { nameId = 47 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test.c" : line 7)
                                 ( "test.c" : line 7 , 3 )
                                 Name { nameId = 51 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argc"
                                      209320289
                                      (NodeInfo
                                         ("test.c" : line 7)
                                         ( "test.c" : line 7 , 4 )
                                         Name { nameId = 50 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test.c" : line 7)
                                   ( "test.c" : line 7 , 4 )
                                   Name { nameId = 52 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test.c" : line 7) ( "test.c" : line 7 , 4 ) Name { nameId = 53 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test.c" : line 7)
                                 ( "test.c" : line 7 , 4 )
                                 Name { nameId = 54 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argv"
                                      249166177
                                      (NodeInfo
                                         ("test.c" : line 7)
                                         ( "test.c" : line 7 , 4 )
                                         Name { nameId = 55 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test.c" : line 7)
                                       ( "test.c" : line 7 , 4 )
                                       Name { nameId = 57 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test.c" : line 7)
                                       ( "test.c" : line 7 , 4 )
                                       Name { nameId = 58 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test.c" : line 7)
                                   ( "test.c" : line 7 , 4 )
                                   Name { nameId = 56 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test.c" : line 7) ( "test.c" : line 7 , 4 ) Name { nameId = 59 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test.c" : line 7) ( "test.c" : line 7 , 1 ) Name { nameId = 60 })
          ]
          Nothing
          []
          (NodeInfo
             ("test.c" : line 7)
             ( "test.c" : line 7 , 4 )
             Name { nameId = 49 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 8)
                           ( "test.c" : line 8 , 3 )
                           Name { nameId = 62 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "yabadabadoo"
                                411916737
                                (NodeInfo
                                   ("test.c" : line 8)
                                   ( "test.c" : line 8 , 11 )
                                   Name { nameId = 61 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 8)
                             ( "test.c" : line 8 , 11 )
                             Name { nameId = 63 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                420
                                (NodeInfo
                                   ("test.c" : line 8)
                                   ( "test.c" : line 8 , 3 )
                                   Name { nameId = 64 })))
                          (NodeInfo
                             ("test.c" : line 8)
                             ( "test.c" : line 8 , 3 )
                             Name { nameId = 65 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 8)
                    ( "test.c" : line 8 , 1 )
                    Name { nameId = 66 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CVar
                          (Ident
                             "yabadabadoo"
                             411916737
                             (NodeInfo
                                ("test.c" : line 9)
                                ( "test.c" : line 9 , 11 )
                                Name { nameId = 67 }))
                          (NodeInfo
                             ("test.c" : line 9)
                             ( "test.c" : line 9 , 11 )
                             Name { nameId = 68 }))
                       (CConst
                          (CIntConst
                             1337
                             (NodeInfo
                                ("test.c" : line 9)
                                ( "test.c" : line 9 , 4 )
                                Name { nameId = 69 })))
                       (NodeInfo
                          ("test.c" : line 9)
                          ( "test.c" : line 9 , 4 )
                          Name { nameId = 70 })))
                 (NodeInfo
                    ("test.c" : line 9)
                    ( "test.c" : line 9 , 1 )
                    Name { nameId = 71 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 10)
                           ( "test.c" : line 10 , 3 )
                           Name { nameId = 73 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "source2"
                                241838806
                                (NodeInfo
                                   ("test.c" : line 10)
                                   ( "test.c" : line 10 , 7 )
                                   Name { nameId = 72 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 10)
                             ( "test.c" : line 10 , 7 )
                             Name { nameId = 74 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                3
                                (NodeInfo
                                   ("test.c" : line 10)
                                   ( "test.c" : line 10 , 1 )
                                   Name { nameId = 75 })))
                          (NodeInfo
                             ("test.c" : line 10)
                             ( "test.c" : line 10 , 1 )
                             Name { nameId = 76 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 10)
                    ( "test.c" : line 10 , 1 )
                    Name { nameId = 77 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 11)
                           ( "test.c" : line 11 , 3 )
                           Name { nameId = 79 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "non_source"
                                442065604
                                (NodeInfo
                                   ("test.c" : line 11)
                                   ( "test.c" : line 11 , 10 )
                                   Name { nameId = 78 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 11)
                             ( "test.c" : line 11 , 10 )
                             Name { nameId = 80 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                2
                                (NodeInfo
                                   ("test.c" : line 11)
                                   ( "test.c" : line 11 , 1 )
                                   Name { nameId = 81 })))
                          (NodeInfo
                             ("test.c" : line 11)
                             ( "test.c" : line 11 , 1 )
                             Name { nameId = 82 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 11)
                    ( "test.c" : line 11 , 1 )
                    Name { nameId = 83 }))
          , CBlockStmt
              (CReturn
                 (Just
                    (CConst
                       (CIntConst
                          0
                          (NodeInfo
                             ("test.c" : line 12)
                             ( "test.c" : line 12 , 1 )
                             Name { nameId = 84 }))))
                 (NodeInfo
                    ("test.c" : line 12)
                    ( "test.c" : line 12 , 1 )
                    Name { nameId = 85 }))
          ]
          (NodeInfo
             ("test.c" : line 7)
             ( "test.c" : line 13 , 1 )
             Name { nameId = 86 }))
       (NodeInfo
          ("test.c" : line 7)
          ( "test.c" : line 13 , 1 )
          Name { nameId = 87 }))
]