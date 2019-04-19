CFDefExt
  (CFunDef
     [ CTypeSpec
         (CIntType
            (NodeInfo
               ("../test2.c" : line 1)
               ( "../test2.c" : line 1 , 3 )
               Name { nameId = 1 }))
     ]
     (CDeclr
        (Just
           (Ident
              "main"
              232419565
              (NodeInfo
                 ("../test2.c" : line 1)
                 ( "../test2.c" : line 1 , 4 )
                 Name { nameId = 0 })))
        [ CFunDeclr
            (Right
               ( [ CDecl
                     [ CTypeSpec
                         (CIntType
                            (NodeInfo
                               ("../test2.c" : line 1)
                               ( "../test2.c" : line 1 , 3 )
                               Name { nameId = 4 }))
                     ]
                     [ ( Just
                           (CDeclr
                              (Just
                                 (Ident
                                    "argc"
                                    209320289
                                    (NodeInfo
                                       ("../test2.c" : line 1)
                                       ( "../test2.c" : line 1 , 4 )
                                       Name { nameId = 3 })))
                              []
                              Nothing
                              []
                              (NodeInfo
                                 ("../test2.c" : line 1)
                                 ( "../test2.c" : line 1 , 4 )
                                 Name { nameId = 5 }))
                       , Nothing
                       , Nothing
                       )
                     ]
                     (NodeInfo
                        ("../test2.c" : line 1)
                        ( "../test2.c" : line 1 , 4 )
                        Name { nameId = 6 })
                 , CDecl
                     [ CTypeSpec
                         (CCharType
                            (NodeInfo
                               ("../test2.c" : line 1)
                               ( "../test2.c" : line 1 , 4 )
                               Name { nameId = 7 }))
                     ]
                     [ ( Just
                           (CDeclr
                              (Just
                                 (Ident
                                    "argv"
                                    249166177
                                    (NodeInfo
                                       ("../test2.c" : line 1)
                                       ( "../test2.c" : line 1 , 4 )
                                       Name { nameId = 8 })))
                              [ CPtrDeclr
                                  []
                                  (NodeInfo
                                     ("../test2.c" : line 1)
                                     ( "../test2.c" : line 1 , 4 )
                                     Name { nameId = 10 })
                              , CPtrDeclr
                                  []
                                  (NodeInfo
                                     ("../test2.c" : line 1)
                                     ( "../test2.c" : line 1 , 4 )
                                     Name { nameId = 11 })
                              ]
                              Nothing
                              []
                              (NodeInfo
                                 ("../test2.c" : line 1)
                                 ( "../test2.c" : line 1 , 4 )
                                 Name { nameId = 9 }))
                       , Nothing
                       , Nothing
                       )
                     ]
                     (NodeInfo
                        ("../test2.c" : line 1)
                        ( "../test2.c" : line 1 , 4 )
                        Name { nameId = 12 })
                 ]
               , False
               ))
            []
            (NodeInfo
               ("../test2.c" : line 1)
               ( "../test2.c" : line 1 , 1 )
               Name { nameId = 13 })
        ]
        Nothing
        []
        (NodeInfo
           ("../test2.c" : line 1)
           ( "../test2.c" : line 1 , 4 )
           Name { nameId = 2 }))
     []
     (CCompound
        []
        [ CBlockDecl
            (CDecl
               [ CTypeSpec
                   (CCharType
                      (NodeInfo
                         ("../test2.c" : line 2)
                         ( "../test2.c" : line 2 , 4 )
                         Name { nameId = 14 }))
               ]
               [ ( Just
                     (CDeclr
                        (Just
                           (Ident
                              "t"
                              116
                              (NodeInfo
                                 ("../test2.c" : line 2)
                                 ( "../test2.c" : line 2 , 1 )
                                 Name { nameId = 15 })))
                        [ CPtrDeclr
                            []
                            (NodeInfo
                               ("../test2.c" : line 2)
                               ( "../test2.c" : line 2 , 1 )
                               Name { nameId = 17 })
                        ]
                        Nothing
                        []
                        (NodeInfo
                           ("../test2.c" : line 2)
                           ( "../test2.c" : line 2 , 1 )
                           Name { nameId = 16 }))
                 , Just
                     (CInitExpr
                        (CConst
                           (CStrConst
                              "asdf"
                              (NodeInfo
                                 ("../test2.c" : line 2)
                                 ( "../test2.c" : line 2 , 6 )
                                 Name { nameId = 18 })))
                        (NodeInfo
                           ("../test2.c" : line 2)
                           ( "../test2.c" : line 2 , 6 )
                           Name { nameId = 19 }))
                 , Nothing
                 )
               ]
               (NodeInfo
                  ("../test2.c" : line 2)
                  ( "../test2.c" : line 2 , 1 )
                  Name { nameId = 20 }))
        , CBlockStmt
            (CExpr
               (Just
                  (CAssign
                     CAssignOp
                     (CUnary
                        CIndOp
                        (CVar
                           (Ident
                              "t"
                              116
                              (NodeInfo
                                 ("../test2.c" : line 3)
                                 ( "../test2.c" : line 3 , 1 )
                                 Name { nameId = 21 }))
                           (NodeInfo
                              ("../test2.c" : line 3)
                              ( "../test2.c" : line 3 , 1 )
                              Name { nameId = 22 }))
                        (NodeInfo
                           ("../test2.c" : line 3)
                           ( "../test2.c" : line 3 , 1 )
                           Name { nameId = 23 }))
                     (CConst
                        (CStrConst
                           "zxcv"
                           (NodeInfo
                              ("../test2.c" : line 3)
                              ( "../test2.c" : line 3 , 6 )
                              Name { nameId = 24 })))
                     (NodeInfo
                        ("../test2.c" : line 3)
                        ( "../test2.c" : line 3 , 6 )
                        Name { nameId = 25 })))
               (NodeInfo
                  ("../test2.c" : line 3)
                  ( "../test2.c" : line 3 , 1 )
                  Name { nameId = 26 }))
        , CBlockDecl
            (CDecl
               [ CTypeSpec
                   (CCharType
                      (NodeInfo
                         ("../test2.c" : line 4)
                         ( "../test2.c" : line 4 , 4 )
                         Name { nameId = 27 }))
               ]
               [ ( Just
                     (CDeclr
                        (Just
                           (Ident
                              "foo"
                              1832934
                              (NodeInfo
                                 ("../test2.c" : line 4)
                                 ( "../test2.c" : line 4 , 3 )
                                 Name { nameId = 28 })))
                        [ CPtrDeclr
                            []
                            (NodeInfo
                               ("../test2.c" : line 4)
                               ( "../test2.c" : line 4 , 3 )
                               Name { nameId = 30 })
                        , CPtrDeclr
                            []
                            (NodeInfo
                               ("../test2.c" : line 4)
                               ( "../test2.c" : line 4 , 3 )
                               Name { nameId = 31 })
                        ]
                        Nothing
                        []
                        (NodeInfo
                           ("../test2.c" : line 4)
                           ( "../test2.c" : line 4 , 3 )
                           Name { nameId = 29 }))
                 , Just
                     (CInitExpr
                        (CVar
                           (Ident
                              "t"
                              116
                              (NodeInfo
                                 ("../test2.c" : line 4)
                                 ( "../test2.c" : line 4 , 1 )
                                 Name { nameId = 32 }))
                           (NodeInfo
                              ("../test2.c" : line 4)
                              ( "../test2.c" : line 4 , 1 )
                              Name { nameId = 33 }))
                        (NodeInfo
                           ("../test2.c" : line 4)
                           ( "../test2.c" : line 4 , 1 )
                           Name { nameId = 34 }))
                 , Nothing
                 )
               ]
               (NodeInfo
                  ("../test2.c" : line 4)
                  ( "../test2.c" : line 4 , 1 )
                  Name { nameId = 35 }))
        , CBlockDecl
            (CDecl
               [ CTypeSpec
                   (CCharType
                      (NodeInfo
                         ("../test2.c" : line 5)
                         ( "../test2.c" : line 5 , 4 )
                         Name { nameId = 36 }))
               ]
               [ ( Just
                     (CDeclr
                        (Just
                           (Ident
                              "bar"
                              1880290
                              (NodeInfo
                                 ("../test2.c" : line 5)
                                 ( "../test2.c" : line 5 , 3 )
                                 Name { nameId = 37 })))
                        [ CPtrDeclr
                            []
                            (NodeInfo
                               ("../test2.c" : line 5)
                               ( "../test2.c" : line 5 , 3 )
                               Name { nameId = 39 })
                        ]
                        Nothing
                        []
                        (NodeInfo
                           ("../test2.c" : line 5)
                           ( "../test2.c" : line 5 , 3 )
                           Name { nameId = 38 }))
                 , Just
                     (CInitExpr
                        (CUnary
                           CIndOp
                           (CVar
                              (Ident
                                 "t"
                                 116
                                 (NodeInfo
                                    ("../test2.c" : line 5)
                                    ( "../test2.c" : line 5 , 1 )
                                    Name { nameId = 40 }))
                              (NodeInfo
                                 ("../test2.c" : line 5)
                                 ( "../test2.c" : line 5 , 1 )
                                 Name { nameId = 41 }))
                           (NodeInfo
                              ("../test2.c" : line 5)
                              ( "../test2.c" : line 5 , 1 )
                              Name { nameId = 42 }))
                        (NodeInfo
                           ("../test2.c" : line 5)
                           ( "../test2.c" : line 5 , 1 )
                           Name { nameId = 43 }))
                 , Nothing
                 )
               ]
               (NodeInfo
                  ("../test2.c" : line 5)
                  ( "../test2.c" : line 5 , 1 )
                  Name { nameId = 44 }))
        , CBlockStmt
            (CIf
               (CBinary
                  CEqOp
                  (CVar
                     (Ident
                        "t"
                        116
                        (NodeInfo
                           ("../test2.c" : line 6)
                           ( "../test2.c" : line 6 , 1 )
                           Name { nameId = 45 }))
                     (NodeInfo
                        ("../test2.c" : line 6)
                        ( "../test2.c" : line 6 , 1 )
                        Name { nameId = 46 }))
                  (CConst
                     (CStrConst
                        "qwer"
                        (NodeInfo
                           ("../test2.c" : line 6)
                           ( "../test2.c" : line 6 , 6 )
                           Name { nameId = 47 })))
                  (NodeInfo
                     ("../test2.c" : line 6)
                     ( "../test2.c" : line 6 , 6 )
                     Name { nameId = 48 }))
               (CCompound
                  []
                  [ CBlockStmt
                      (CReturn
                         (Just
                            (CConst
                               (CIntConst
                                  1
                                  (NodeInfo
                                     ("../test2.c" : line 7)
                                     ( "../test2.c" : line 7 , 1 )
                                     Name { nameId = 49 }))))
                         (NodeInfo
                            ("../test2.c" : line 7)
                            ( "../test2.c" : line 7 , 1 )
                            Name { nameId = 50 }))
                  ]
                  (NodeInfo
                     ("../test2.c" : line 6)
                     ( "../test2.c" : line 8 , 1 )
                     Name { nameId = 51 }))
               (Just
                  (CIf
                     (CBinary
                        CEqOp
                        (CVar
                           (Ident
                              "t"
                              116
                              (NodeInfo
                                 ("../test2.c" : line 8)
                                 ( "../test2.c" : line 8 , 1 )
                                 Name { nameId = 52 }))
                           (NodeInfo
                              ("../test2.c" : line 8)
                              ( "../test2.c" : line 8 , 1 )
                              Name { nameId = 53 }))
                        (CConst
                           (CStrConst
                              "jklo"
                              (NodeInfo
                                 ("../test2.c" : line 8)
                                 ( "../test2.c" : line 8 , 6 )
                                 Name { nameId = 54 })))
                        (NodeInfo
                           ("../test2.c" : line 8)
                           ( "../test2.c" : line 8 , 6 )
                           Name { nameId = 55 }))
                     (CCompound
                        []
                        [ CBlockStmt
                            (CReturn
                               (Just
                                  (CConst
                                     (CIntConst
                                        0
                                        (NodeInfo
                                           ("../test2.c" : line 9)
                                           ( "../test2.c" : line 9 , 1 )
                                           Name { nameId = 56 }))))
                               (NodeInfo
                                  ("../test2.c" : line 9)
                                  ( "../test2.c" : line 9 , 1 )
                                  Name { nameId = 57 }))
                        ]
                        (NodeInfo
                           ("../test2.c" : line 8)
                           ( "../test2.c" : line 10 , 1 )
                           Name { nameId = 58 }))
                     (Just
                        (CCompound
                           []
                           [ CBlockStmt
                               (CReturn
                                  (Just
                                     (CConst
                                        (CIntConst
                                           2
                                           (NodeInfo
                                              ("../test2.c" : line 11)
                                              ( "../test2.c" : line 11 , 1 )
                                              Name { nameId = 59 }))))
                                  (NodeInfo
                                     ("../test2.c" : line 11)
                                     ( "../test2.c" : line 11 , 1 )
                                     Name { nameId = 60 }))
                           ]
                           (NodeInfo
                              ("../test2.c" : line 10)
                              ( "../test2.c" : line 12 , 1 )
                              Name { nameId = 61 })))
                     (NodeInfo
                        ("../test2.c" : line 8)
                        ( "../test2.c" : line 12 , 1 )
                        Name { nameId = 62 })))
               (NodeInfo
                  ("../test2.c" : line 6)
                  ( "../test2.c" : line 12 , 1 )
                  Name { nameId = 63 }))
        , CBlockStmt
            (CFor
               (Right
                  (CDecl
                     [ CTypeSpec
                         (CIntType
                            (NodeInfo
                               ("../test2.c" : line 13)
                               ( "../test2.c" : line 13 , 3 )
                               Name { nameId = 65 }))
                     ]
                     [ ( Just
                           (CDeclr
                              (Just
                                 (Ident
                                    "i"
                                    105
                                    (NodeInfo
                                       ("../test2.c" : line 13)
                                       ( "../test2.c" : line 13 , 1 )
                                       Name { nameId = 64 })))
                              []
                              Nothing
                              []
                              (NodeInfo
                                 ("../test2.c" : line 13)
                                 ( "../test2.c" : line 13 , 1 )
                                 Name { nameId = 66 }))
                       , Just
                           (CInitExpr
                              (CConst
                                 (CIntConst
                                    0
                                    (NodeInfo
                                       ("../test2.c" : line 13)
                                       ( "../test2.c" : line 13 , 1 )
                                       Name { nameId = 67 })))
                              (NodeInfo
                                 ("../test2.c" : line 13)
                                 ( "../test2.c" : line 13 , 1 )
                                 Name { nameId = 68 }))
                       , Nothing
                       )
                     ]
                     (NodeInfo
                        ("../test2.c" : line 13)
                        ( "../test2.c" : line 13 , 1 )
                        Name { nameId = 69 })))
               (Just
                  (CBinary
                     CLeOp
                     (CVar
                        (Ident
                           "i"
                           105
                           (NodeInfo
                              ("../test2.c" : line 13)
                              ( "../test2.c" : line 13 , 1 )
                              Name { nameId = 70 }))
                        (NodeInfo
                           ("../test2.c" : line 13)
                           ( "../test2.c" : line 13 , 1 )
                           Name { nameId = 71 }))
                     (CConst
                        (CIntConst
                           10
                           (NodeInfo
                              ("../test2.c" : line 13)
                              ( "../test2.c" : line 13 , 2 )
                              Name { nameId = 72 })))
                     (NodeInfo
                        ("../test2.c" : line 13)
                        ( "../test2.c" : line 13 , 2 )
                        Name { nameId = 73 })))
               (Just
                  (CUnary
                     CPostIncOp
                     (CVar
                        (Ident
                           "i"
                           105
                           (NodeInfo
                              ("../test2.c" : line 13)
                              ( "../test2.c" : line 13 , 1 )
                              Name { nameId = 74 }))
                        (NodeInfo
                           ("../test2.c" : line 13)
                           ( "../test2.c" : line 13 , 1 )
                           Name { nameId = 75 }))
                     (NodeInfo
                        ("../test2.c" : line 13)
                        ( "../test2.c" : line 13 , 2 )
                        Name { nameId = 76 })))
               (CCompound
                  []
                  [ CBlockStmt
                      (CExpr
                         (Just
                            (CAssign
                               CAssignOp
                               (CUnary
                                  CIndOp
                                  (CVar
                                     (Ident
                                        "t"
                                        116
                                        (NodeInfo
                                           ("../test2.c" : line 14)
                                           ( "../test2.c" : line 14 , 1 )
                                           Name { nameId = 77 }))
                                     (NodeInfo
                                        ("../test2.c" : line 14)
                                        ( "../test2.c" : line 14 , 1 )
                                        Name { nameId = 78 }))
                                  (NodeInfo
                                     ("../test2.c" : line 14)
                                     ( "../test2.c" : line 14 , 1 )
                                     Name { nameId = 79 }))
                               (CConst
                                  (CStrConst
                                     "qwer"
                                     (NodeInfo
                                        ("../test2.c" : line 14)
                                        ( "../test2.c" : line 14 , 6 )
                                        Name { nameId = 80 })))
                               (NodeInfo
                                  ("../test2.c" : line 14)
                                  ( "../test2.c" : line 14 , 6 )
                                  Name { nameId = 81 })))
                         (NodeInfo
                            ("../test2.c" : line 14)
                            ( "../test2.c" : line 14 , 1 )
                            Name { nameId = 82 }))
                  ]
                  (NodeInfo
                     ("../test2.c" : line 13)
                     ( "../test2.c" : line 15 , 1 )
                     Name { nameId = 83 }))
               (NodeInfo
                  ("../test2.c" : line 13)
                  ( "../test2.c" : line 15 , 1 )
                  Name { nameId = 84 }))
        , CBlockDecl
            (CDecl
               [ CTypeSpec
                   (CIntType
                      (NodeInfo
                         ("../test2.c" : line 16)
                         ( "../test2.c" : line 16 , 3 )
                         Name { nameId = 86 }))
               ]
               [ ( Just
                     (CDeclr
                        (Just
                           (Ident
                              "k"
                              107
                              (NodeInfo
                                 ("../test2.c" : line 16)
                                 ( "../test2.c" : line 16 , 1 )
                                 Name { nameId = 85 })))
                        []
                        Nothing
                        []
                        (NodeInfo
                           ("../test2.c" : line 16)
                           ( "../test2.c" : line 16 , 1 )
                           Name { nameId = 87 }))
                 , Just
                     (CInitExpr
                        (CConst
                           (CIntConst
                              0
                              (NodeInfo
                                 ("../test2.c" : line 16)
                                 ( "../test2.c" : line 16 , 1 )
                                 Name { nameId = 88 })))
                        (NodeInfo
                           ("../test2.c" : line 16)
                           ( "../test2.c" : line 16 , 1 )
                           Name { nameId = 89 }))
                 , Nothing
                 )
               ]
               (NodeInfo
                  ("../test2.c" : line 16)
                  ( "../test2.c" : line 16 , 1 )
                  Name { nameId = 90 }))
        , CBlockStmt
            (CWhile
               (CBinary
                  CLeOp
                  (CVar
                     (Ident
                        "k"
                        107
                        (NodeInfo
                           ("../test2.c" : line 17)
                           ( "../test2.c" : line 17 , 1 )
                           Name { nameId = 91 }))
                     (NodeInfo
                        ("../test2.c" : line 17)
                        ( "../test2.c" : line 17 , 1 )
                        Name { nameId = 92 }))
                  (CConst
                     (CIntConst
                        10
                        (NodeInfo
                           ("../test2.c" : line 17)
                           ( "../test2.c" : line 17 , 2 )
                           Name { nameId = 93 })))
                  (NodeInfo
                     ("../test2.c" : line 17)
                     ( "../test2.c" : line 17 , 2 )
                     Name { nameId = 94 }))
               (CCompound
                  []
                  [ CBlockStmt
                      (CExpr
                         (Just
                            (CUnary
                               CPostIncOp
                               (CVar
                                  (Ident
                                     "k"
                                     107
                                     (NodeInfo
                                        ("../test2.c" : line 18)
                                        ( "../test2.c" : line 18 , 1 )
                                        Name { nameId = 95 }))
                                  (NodeInfo
                                     ("../test2.c" : line 18)
                                     ( "../test2.c" : line 18 , 1 )
                                     Name { nameId = 96 }))
                               (NodeInfo
                                  ("../test2.c" : line 18)
                                  ( "../test2.c" : line 18 , 2 )
                                  Name { nameId = 97 })))
                         (NodeInfo
                            ("../test2.c" : line 18)
                            ( "../test2.c" : line 18 , 1 )
                            Name { nameId = 98 }))
                  , CBlockStmt
                      (CExpr
                         (Just
                            (CAssign
                               CAssignOp
                               (CUnary
                                  CIndOp
                                  (CVar
                                     (Ident
                                        "t"
                                        116
                                        (NodeInfo
                                           ("../test2.c" : line 19)
                                           ( "../test2.c" : line 19 , 1 )
                                           Name { nameId = 99 }))
                                     (NodeInfo
                                        ("../test2.c" : line 19)
                                        ( "../test2.c" : line 19 , 1 )
                                        Name { nameId = 100 }))
                                  (NodeInfo
                                     ("../test2.c" : line 19)
                                     ( "../test2.c" : line 19 , 1 )
                                     Name { nameId = 101 }))
                               (CConst
                                  (CStrConst
                                     "jkl;"
                                     (NodeInfo
                                        ("../test2.c" : line 19)
                                        ( "../test2.c" : line 19 , 6 )
                                        Name { nameId = 102 })))
                               (NodeInfo
                                  ("../test2.c" : line 19)
                                  ( "../test2.c" : line 19 , 6 )
                                  Name { nameId = 103 })))
                         (NodeInfo
                            ("../test2.c" : line 19)
                            ( "../test2.c" : line 19 , 1 )
                            Name { nameId = 104 }))
                  ]
                  (NodeInfo
                     ("../test2.c" : line 17)
                     ( "../test2.c" : line 20 , 1 )
                     Name { nameId = 105 }))
               False
               (NodeInfo
                  ("../test2.c" : line 17)
                  ( "../test2.c" : line 20 , 1 )
                  Name { nameId = 106 }))
        ]
        (NodeInfo
           ("../test2.c" : line 1)
           ( "../test2.c" : line 21 , 1 )
           Name { nameId = 107 }))
     (NodeInfo
        ("../test2.c" : line 1)
        ( "../test2.c" : line 21 , 1 )
        Name { nameId = 108 }))