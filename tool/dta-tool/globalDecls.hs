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
                      "f"
                      102
                      (NodeInfo
                         ("test.c" : line 1)
                         ( "test.c" : line 1 , 1 )
                         Name { nameId = 0 })))
                [ CFunDeclr
                    (Right
                       ( [ CDecl
                             [ CTypeSpec
                                 (CCharType
                                    (NodeInfo
                                       ("test.c" : line 1)
                                       ( "test.c" : line 1 , 4 )
                                       Name { nameId = 4 }))
                             ]
                             [ ( Just
                                   (CDeclr
                                      (Just
                                         (Ident
                                            "q"
                                            113
                                            (NodeInfo
                                               ("test.c" : line 1)
                                               ( "test.c" : line 1 , 1 )
                                               Name { nameId = 3 })))
                                      []
                                      Nothing
                                      []
                                      (NodeInfo
                                         ("test.c" : line 1)
                                         ( "test.c" : line 1 , 1 )
                                         Name { nameId = 5 }))
                               , Nothing
                               , Nothing
                               )
                             ]
                             (NodeInfo
                                ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 6 })
                         , CDecl
                             [ CTypeSpec
                                 (CCharType
                                    (NodeInfo
                                       ("test.c" : line 1)
                                       ( "test.c" : line 1 , 4 )
                                       Name { nameId = 8 }))
                             ]
                             [ ( Just
                                   (CDeclr
                                      (Just
                                         (Ident
                                            "l"
                                            108
                                            (NodeInfo
                                               ("test.c" : line 1)
                                               ( "test.c" : line 1 , 1 )
                                               Name { nameId = 7 })))
                                      []
                                      Nothing
                                      []
                                      (NodeInfo
                                         ("test.c" : line 1)
                                         ( "test.c" : line 1 , 1 )
                                         Name { nameId = 9 }))
                               , Nothing
                               , Nothing
                               )
                             ]
                             (NodeInfo
                                ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 10 })
                         ]
                       , False
                       ))
                    []
                    (NodeInfo
                       ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 11 })
                ]
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 1) ( "test.c" : line 1 , 1 ) Name { nameId = 2 }))
         , Nothing
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 1)
          ( "test.c" : line 1 , 1 )
          Name { nameId = 12 }))
, CDeclExt
    (CDecl
       [ CStorageSpec
           (CExtern
              (NodeInfo
                 ("test.c" : line 2)
                 ( "test.c" : line 2 , 6 )
                 Name { nameId = 13 }))
       , CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 2)
                 ( "test.c" : line 2 , 4 )
                 Name { nameId = 15 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "k"
                      107
                      (NodeInfo
                         ("test.c" : line 2)
                         ( "test.c" : line 2 , 1 )
                         Name { nameId = 14 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 2)
                   ( "test.c" : line 2 , 1 )
                   Name { nameId = 16 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 2)
                         ( "test.c" : line 2 , 3 )
                         Name { nameId = 17 })))
                (NodeInfo
                   ("test.c" : line 2)
                   ( "test.c" : line 2 , 3 )
                   Name { nameId = 18 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 2)
          ( "test.c" : line 2 , 1 )
          Name { nameId = 19 }))
, CDeclExt
    (CDecl
       [ CStorageSpec
           (CExtern
              (NodeInfo
                 ("test.c" : line 3)
                 ( "test.c" : line 3 , 6 )
                 Name { nameId = 20 }))
       , CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 3)
                 ( "test.c" : line 3 , 4 )
                 Name { nameId = 22 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "r"
                      114
                      (NodeInfo
                         ("test.c" : line 3)
                         ( "test.c" : line 3 , 1 )
                         Name { nameId = 21 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 3)
                   ( "test.c" : line 3 , 1 )
                   Name { nameId = 23 }))
         , Just
             (CInitExpr
                (CCall
                   (CVar
                      (Ident
                         "f"
                         102
                         (NodeInfo
                            ("test.c" : line 3)
                            ( "test.c" : line 3 , 1 )
                            Name { nameId = 24 }))
                      (NodeInfo
                         ("test.c" : line 3)
                         ( "test.c" : line 3 , 1 )
                         Name { nameId = 25 }))
                   [ CVar
                       (Ident
                          "y"
                          121
                          (NodeInfo
                             ("test.c" : line 3)
                             ( "test.c" : line 3 , 1 )
                             Name { nameId = 26 }))
                       (NodeInfo
                          ("test.c" : line 3) ( "test.c" : line 3 , 1 ) Name { nameId = 27 })
                   , CVar
                       (Ident
                          "z"
                          122
                          (NodeInfo
                             ("test.c" : line 3)
                             ( "test.c" : line 3 , 1 )
                             Name { nameId = 28 }))
                       (NodeInfo
                          ("test.c" : line 3) ( "test.c" : line 3 , 1 ) Name { nameId = 29 })
                   ]
                   (NodeInfo
                      ("test.c" : line 3)
                      ( "test.c" : line 3 , 1 )
                      Name { nameId = 30 }))
                (NodeInfo
                   ("test.c" : line 3)
                   ( "test.c" : line 3 , 1 )
                   Name { nameId = 31 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 3)
          ( "test.c" : line 3 , 1 )
          Name { nameId = 32 }))
, CDeclExt
    (CDecl
       [ CStorageSpec
           (CExtern
              (NodeInfo
                 ("test.c" : line 4)
                 ( "test.c" : line 4 , 6 )
                 Name { nameId = 33 }))
       , CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 4)
                 ( "test.c" : line 4 , 4 )
                 Name { nameId = 35 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "t"
                      116
                      (NodeInfo
                         ("test.c" : line 4)
                         ( "test.c" : line 4 , 1 )
                         Name { nameId = 34 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 4)
                   ( "test.c" : line 4 , 1 )
                   Name { nameId = 36 }))
         , Nothing
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 4)
          ( "test.c" : line 4 , 1 )
          Name { nameId = 37 }))
, CDeclExt
    (CDecl
       [ CStorageSpec
           (CExtern
              (NodeInfo
                 ("test.c" : line 5)
                 ( "test.c" : line 5 , 6 )
                 Name { nameId = 38 }))
       , CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 5)
                 ( "test.c" : line 5 , 4 )
                 Name { nameId = 40 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "y"
                      121
                      (NodeInfo
                         ("test.c" : line 5)
                         ( "test.c" : line 5 , 1 )
                         Name { nameId = 39 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 5)
                   ( "test.c" : line 5 , 1 )
                   Name { nameId = 41 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 5)
                         ( "test.c" : line 5 , 3 )
                         Name { nameId = 42 })))
                (NodeInfo
                   ("test.c" : line 5)
                   ( "test.c" : line 5 , 3 )
                   Name { nameId = 43 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 5)
          ( "test.c" : line 5 , 1 )
          Name { nameId = 44 }))
, CDeclExt
    (CDecl
       [ CStorageSpec
           (CExtern
              (NodeInfo
                 ("test.c" : line 6)
                 ( "test.c" : line 6 , 6 )
                 Name { nameId = 45 }))
       , CTypeSpec
           (CCharType
              (NodeInfo
                 ("test.c" : line 6)
                 ( "test.c" : line 6 , 4 )
                 Name { nameId = 47 }))
       ]
       [ ( Just
             (CDeclr
                (Just
                   (Ident
                      "z"
                      122
                      (NodeInfo
                         ("test.c" : line 6)
                         ( "test.c" : line 6 , 1 )
                         Name { nameId = 46 })))
                []
                Nothing
                []
                (NodeInfo
                   ("test.c" : line 6)
                   ( "test.c" : line 6 , 1 )
                   Name { nameId = 48 }))
         , Just
             (CInitExpr
                (CConst
                   (CStrConst
                      "a"
                      (NodeInfo
                         ("test.c" : line 6)
                         ( "test.c" : line 6 , 3 )
                         Name { nameId = 49 })))
                (NodeInfo
                   ("test.c" : line 6)
                   ( "test.c" : line 6 , 3 )
                   Name { nameId = 50 }))
         , Nothing
         )
       ]
       (NodeInfo
          ("test.c" : line 6)
          ( "test.c" : line 6 , 1 )
          Name { nameId = 51 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test.c" : line 7)
                 ( "test.c" : line 7 , 3 )
                 Name { nameId = 53 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test.c" : line 7)
                   ( "test.c" : line 7 , 4 )
                   Name { nameId = 52 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test.c" : line 7)
                                 ( "test.c" : line 7 , 3 )
                                 Name { nameId = 56 }))
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
                                         Name { nameId = 55 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test.c" : line 7)
                                   ( "test.c" : line 7 , 4 )
                                   Name { nameId = 57 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test.c" : line 7) ( "test.c" : line 7 , 4 ) Name { nameId = 58 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test.c" : line 7)
                                 ( "test.c" : line 7 , 4 )
                                 Name { nameId = 59 }))
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
                                         Name { nameId = 60 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test.c" : line 7)
                                       ( "test.c" : line 7 , 4 )
                                       Name { nameId = 62 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test.c" : line 7)
                                       ( "test.c" : line 7 , 4 )
                                       Name { nameId = 63 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test.c" : line 7)
                                   ( "test.c" : line 7 , 4 )
                                   Name { nameId = 61 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test.c" : line 7) ( "test.c" : line 7 , 4 ) Name { nameId = 64 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test.c" : line 7) ( "test.c" : line 7 , 1 ) Name { nameId = 65 })
          ]
          Nothing
          []
          (NodeInfo
             ("test.c" : line 7)
             ( "test.c" : line 7 , 4 )
             Name { nameId = 54 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 9)
                           ( "test.c" : line 9 , 3 )
                           Name { nameId = 67 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "yabadabadoo"
                                411916737
                                (NodeInfo
                                   ("test.c" : line 9)
                                   ( "test.c" : line 9 , 11 )
                                   Name { nameId = 66 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 9)
                             ( "test.c" : line 9 , 11 )
                             Name { nameId = 68 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                420
                                (NodeInfo
                                   ("test.c" : line 9)
                                   ( "test.c" : line 9 , 3 )
                                   Name { nameId = 69 })))
                          (NodeInfo
                             ("test.c" : line 9)
                             ( "test.c" : line 9 , 3 )
                             Name { nameId = 70 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 9)
                    ( "test.c" : line 9 , 1 )
                    Name { nameId = 71 }))
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
                                ("test.c" : line 10)
                                ( "test.c" : line 10 , 11 )
                                Name { nameId = 72 }))
                          (NodeInfo
                             ("test.c" : line 10)
                             ( "test.c" : line 10 , 11 )
                             Name { nameId = 73 }))
                       (CConst
                          (CIntConst
                             1337
                             (NodeInfo
                                ("test.c" : line 10)
                                ( "test.c" : line 10 , 4 )
                                Name { nameId = 74 })))
                       (NodeInfo
                          ("test.c" : line 10)
                          ( "test.c" : line 10 , 4 )
                          Name { nameId = 75 })))
                 (NodeInfo
                    ("test.c" : line 10)
                    ( "test.c" : line 10 , 1 )
                    Name { nameId = 76 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 11)
                           ( "test.c" : line 11 , 3 )
                           Name { nameId = 78 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "source2"
                                241838806
                                (NodeInfo
                                   ("test.c" : line 11)
                                   ( "test.c" : line 11 , 7 )
                                   Name { nameId = 77 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 11)
                             ( "test.c" : line 11 , 7 )
                             Name { nameId = 79 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                3
                                (NodeInfo
                                   ("test.c" : line 11)
                                   ( "test.c" : line 11 , 1 )
                                   Name { nameId = 80 })))
                          (NodeInfo
                             ("test.c" : line 11)
                             ( "test.c" : line 11 , 1 )
                             Name { nameId = 81 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 11)
                    ( "test.c" : line 11 , 1 )
                    Name { nameId = 82 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test.c" : line 12)
                           ( "test.c" : line 12 , 3 )
                           Name { nameId = 84 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "non_source"
                                442065604
                                (NodeInfo
                                   ("test.c" : line 12)
                                   ( "test.c" : line 12 , 10 )
                                   Name { nameId = 83 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test.c" : line 12)
                             ( "test.c" : line 12 , 10 )
                             Name { nameId = 85 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                2
                                (NodeInfo
                                   ("test.c" : line 12)
                                   ( "test.c" : line 12 , 1 )
                                   Name { nameId = 86 })))
                          (NodeInfo
                             ("test.c" : line 12)
                             ( "test.c" : line 12 , 1 )
                             Name { nameId = 87 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test.c" : line 12)
                    ( "test.c" : line 12 , 1 )
                    Name { nameId = 88 }))
          , CBlockStmt
              (CReturn
                 (Just
                    (CConst
                       (CIntConst
                          0
                          (NodeInfo
                             ("test.c" : line 13)
                             ( "test.c" : line 13 , 1 )
                             Name { nameId = 89 }))))
                 (NodeInfo
                    ("test.c" : line 13)
                    ( "test.c" : line 13 , 1 )
                    Name { nameId = 90 }))
          ]
          (NodeInfo
             ("test.c" : line 8)
             ( "test.c" : line 14 , 1 )
             Name { nameId = 91 }))
       (NodeInfo
          ("test.c" : line 7)
          ( "test.c" : line 14 , 1 )
          Name { nameId = 92 }))
]