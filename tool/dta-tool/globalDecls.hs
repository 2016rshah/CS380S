[ CFDefExt
    (CFunDef
       [ CTypeSpec
           (CVoidType
              (NodeInfo
                 ("test2.c" : line 1)
                 ( "test2.c" : line 1 , 4 )
                 Name { nameId = 1 }))
       ]
       (CDeclr
          (Just
             (Ident
                "f"
                102
                (NodeInfo
                   ("test2.c" : line 1)
                   ( "test2.c" : line 1 , 1 )
                   Name { nameId = 0 })))
          [ CFunDeclr
              (Right ( [] , False ))
              []
              (NodeInfo
                 ("test2.c" : line 1)
                 ( "test2.c" : line 1 , 1 )
                 Name { nameId = 3 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 1)
             ( "test2.c" : line 1 , 1 )
             Name { nameId = 2 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 3)
                           ( "test2.c" : line 3 , 3 )
                           Name { nameId = 5 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "testVar"
                                247047114
                                (NodeInfo
                                   ("test2.c" : line 3)
                                   ( "test2.c" : line 3 , 7 )
                                   Name { nameId = 4 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 3)
                             ( "test2.c" : line 3 , 7 )
                             Name { nameId = 6 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                3
                                (NodeInfo
                                   ("test2.c" : line 3)
                                   ( "test2.c" : line 3 , 1 )
                                   Name { nameId = 7 })))
                          (NodeInfo
                             ("test2.c" : line 3)
                             ( "test2.c" : line 3 , 1 )
                             Name { nameId = 8 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 3)
                    ( "test2.c" : line 3 , 1 )
                    Name { nameId = 9 }))
          ]
          (NodeInfo
             ("test2.c" : line 2)
             ( "test2.c" : line 4 , 1 )
             Name { nameId = 10 }))
       (NodeInfo
          ("test2.c" : line 1)
          ( "test2.c" : line 4 , 1 )
          Name { nameId = 11 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 3 )
                 Name { nameId = 13 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test2.c" : line 5)
                   ( "test2.c" : line 5 , 4 )
                   Name { nameId = 12 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 5)
                                 ( "test2.c" : line 5 , 3 )
                                 Name { nameId = 16 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argc"
                                      209320289
                                      (NodeInfo
                                         ("test2.c" : line 5)
                                         ( "test2.c" : line 5 , 4 )
                                         Name { nameId = 15 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 5)
                                   ( "test2.c" : line 5 , 4 )
                                   Name { nameId = 17 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 5)
                          ( "test2.c" : line 5 , 4 )
                          Name { nameId = 18 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test2.c" : line 5)
                                 ( "test2.c" : line 5 , 4 )
                                 Name { nameId = 19 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argv"
                                      249166177
                                      (NodeInfo
                                         ("test2.c" : line 5)
                                         ( "test2.c" : line 5 , 4 )
                                         Name { nameId = 20 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 5)
                                       ( "test2.c" : line 5 , 4 )
                                       Name { nameId = 22 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 5)
                                       ( "test2.c" : line 5 , 4 )
                                       Name { nameId = 23 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 5)
                                   ( "test2.c" : line 5 , 4 )
                                   Name { nameId = 21 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 5)
                          ( "test2.c" : line 5 , 4 )
                          Name { nameId = 24 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 1 )
                 Name { nameId = 25 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 5)
             ( "test2.c" : line 5 , 4 )
             Name { nameId = 14 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 7)
                           ( "test2.c" : line 7 , 4 )
                           Name { nameId = 26 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 7)
                                   ( "test2.c" : line 7 , 1 )
                                   Name { nameId = 27 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 7)
                                 ( "test2.c" : line 7 , 1 )
                                 Name { nameId = 29 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 1 )
                             Name { nameId = 28 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "asdf"
                                (NodeInfo
                                   ("test2.c" : line 7)
                                   ( "test2.c" : line 7 , 6 )
                                   Name { nameId = 30 })))
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 6 )
                             Name { nameId = 31 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 7)
                    ( "test2.c" : line 7 , 1 )
                    Name { nameId = 32 }))
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
                                   ("test2.c" : line 8)
                                   ( "test2.c" : line 8 , 1 )
                                   Name { nameId = 33 }))
                             (NodeInfo
                                ("test2.c" : line 8)
                                ( "test2.c" : line 8 , 1 )
                                Name { nameId = 34 }))
                          (NodeInfo
                             ("test2.c" : line 8)
                             ( "test2.c" : line 8 , 1 )
                             Name { nameId = 35 }))
                       (CConst
                          (CStrConst
                             "zxcv"
                             (NodeInfo
                                ("test2.c" : line 8)
                                ( "test2.c" : line 8 , 6 )
                                Name { nameId = 36 })))
                       (NodeInfo
                          ("test2.c" : line 8)
                          ( "test2.c" : line 8 , 6 )
                          Name { nameId = 37 })))
                 (NodeInfo
                    ("test2.c" : line 8)
                    ( "test2.c" : line 8 , 1 )
                    Name { nameId = 38 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 9)
                           ( "test2.c" : line 9 , 4 )
                           Name { nameId = 39 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "foo"
                                1832934
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 3 )
                                   Name { nameId = 40 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 9)
                                 ( "test2.c" : line 9 , 3 )
                                 Name { nameId = 42 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 9)
                             ( "test2.c" : line 9 , 3 )
                             Name { nameId = 41 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "SOURCE"
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 8 )
                                   Name { nameId = 43 })))
                          (NodeInfo
                             ("test2.c" : line 9)
                             ( "test2.c" : line 9 , 8 )
                             Name { nameId = 44 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 9)
                    ( "test2.c" : line 9 , 1 )
                    Name { nameId = 45 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 10)
                           ( "test2.c" : line 10 , 4 )
                           Name { nameId = 46 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "bar"
                                1880290
                                (NodeInfo
                                   ("test2.c" : line 10)
                                   ( "test2.c" : line 10 , 3 )
                                   Name { nameId = 47 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 10)
                                 ( "test2.c" : line 10 , 3 )
                                 Name { nameId = 49 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 10)
                             ( "test2.c" : line 10 , 3 )
                             Name { nameId = 48 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "SOURCE"
                                (NodeInfo
                                   ("test2.c" : line 10)
                                   ( "test2.c" : line 10 , 8 )
                                   Name { nameId = 50 })))
                          (NodeInfo
                             ("test2.c" : line 10)
                             ( "test2.c" : line 10 , 8 )
                             Name { nameId = 51 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 10)
                    ( "test2.c" : line 10 , 1 )
                    Name { nameId = 52 }))
          , CBlockStmt
              (CIf
                 (CBinary
                    CEqOp
                    (CVar
                       (Ident
                          "t"
                          116
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 1 )
                             Name { nameId = 53 }))
                       (NodeInfo
                          ("test2.c" : line 11)
                          ( "test2.c" : line 11 , 1 )
                          Name { nameId = 54 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 6 )
                             Name { nameId = 55 })))
                    (NodeInfo
                       ("test2.c" : line 11)
                       ( "test2.c" : line 11 , 6 )
                       Name { nameId = 56 }))
                 (CCompound
                    []
                    [ CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 13)
                                       ( "test2.c" : line 13 , 1 )
                                       Name { nameId = 57 }))))
                           (NodeInfo
                              ("test2.c" : line 13)
                              ( "test2.c" : line 13 , 1 )
                              Name { nameId = 58 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 12)
                       ( "test2.c" : line 14 , 1 )
                       Name { nameId = 59 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 15)
                                   ( "test2.c" : line 15 , 1 )
                                   Name { nameId = 60 }))
                             (NodeInfo
                                ("test2.c" : line 15)
                                ( "test2.c" : line 15 , 1 )
                                Name { nameId = 61 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 15)
                                   ( "test2.c" : line 15 , 6 )
                                   Name { nameId = 62 })))
                          (NodeInfo
                             ("test2.c" : line 15)
                             ( "test2.c" : line 15 , 6 )
                             Name { nameId = 63 }))
                       (CCompound
                          []
                          [ CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 17)
                                             ( "test2.c" : line 17 , 1 )
                                             Name { nameId = 64 }))))
                                 (NodeInfo
                                    ("test2.c" : line 17)
                                    ( "test2.c" : line 17 , 1 )
                                    Name { nameId = 65 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 16)
                             ( "test2.c" : line 18 , 1 )
                             Name { nameId = 66 }))
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
                                                ("test2.c" : line 21)
                                                ( "test2.c" : line 21 , 1 )
                                                Name { nameId = 67 }))))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 1 )
                                       Name { nameId = 68 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 20)
                                ( "test2.c" : line 22 , 1 )
                                Name { nameId = 69 })))
                       (NodeInfo
                          ("test2.c" : line 15)
                          ( "test2.c" : line 22 , 1 )
                          Name { nameId = 70 })))
                 (NodeInfo
                    ("test2.c" : line 11)
                    ( "test2.c" : line 22 , 1 )
                    Name { nameId = 71 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 23)
                                 ( "test2.c" : line 23 , 3 )
                                 Name { nameId = 73 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 23)
                                         ( "test2.c" : line 23 , 1 )
                                         Name { nameId = 72 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 23)
                                   ( "test2.c" : line 23 , 1 )
                                   Name { nameId = 74 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CIntConst
                                      0
                                      (NodeInfo
                                         ("test2.c" : line 23)
                                         ( "test2.c" : line 23 , 1 )
                                         Name { nameId = 75 })))
                                (NodeInfo
                                   ("test2.c" : line 23)
                                   ( "test2.c" : line 23 , 1 )
                                   Name { nameId = 76 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 23)
                          ( "test2.c" : line 23 , 1 )
                          Name { nameId = 77 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 23)
                                ( "test2.c" : line 23 , 1 )
                                Name { nameId = 78 }))
                          (NodeInfo
                             ("test2.c" : line 23)
                             ( "test2.c" : line 23 , 1 )
                             Name { nameId = 79 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 23)
                                ( "test2.c" : line 23 , 2 )
                                Name { nameId = 80 })))
                       (NodeInfo
                          ("test2.c" : line 23)
                          ( "test2.c" : line 23 , 2 )
                          Name { nameId = 81 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 23)
                                ( "test2.c" : line 23 , 1 )
                                Name { nameId = 82 }))
                          (NodeInfo
                             ("test2.c" : line 23)
                             ( "test2.c" : line 23 , 1 )
                             Name { nameId = 83 }))
                       (NodeInfo
                          ("test2.c" : line 23)
                          ( "test2.c" : line 23 , 2 )
                          Name { nameId = 84 })))
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
                                             ("test2.c" : line 25)
                                             ( "test2.c" : line 25 , 1 )
                                             Name { nameId = 85 }))
                                       (NodeInfo
                                          ("test2.c" : line 25)
                                          ( "test2.c" : line 25 , 1 )
                                          Name { nameId = 86 }))
                                    (NodeInfo
                                       ("test2.c" : line 25)
                                       ( "test2.c" : line 25 , 1 )
                                       Name { nameId = 87 }))
                                 (CConst
                                    (CStrConst
                                       "qwer"
                                       (NodeInfo
                                          ("test2.c" : line 25)
                                          ( "test2.c" : line 25 , 6 )
                                          Name { nameId = 88 })))
                                 (NodeInfo
                                    ("test2.c" : line 25)
                                    ( "test2.c" : line 25 , 6 )
                                    Name { nameId = 89 })))
                           (NodeInfo
                              ("test2.c" : line 25)
                              ( "test2.c" : line 25 , 1 )
                              Name { nameId = 90 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 24)
                       ( "test2.c" : line 26 , 1 )
                       Name { nameId = 91 }))
                 (NodeInfo
                    ("test2.c" : line 23)
                    ( "test2.c" : line 26 , 1 )
                    Name { nameId = 92 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 27)
                           ( "test2.c" : line 27 , 4 )
                           Name { nameId = 93 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 27)
                                   ( "test2.c" : line 27 , 1 )
                                   Name { nameId = 94 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 27)
                                 ( "test2.c" : line 27 , 1 )
                                 Name { nameId = 96 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 27)
                             ( "test2.c" : line 27 , 1 )
                             Name { nameId = 95 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "PRESERVING"
                                (NodeInfo
                                   ("test2.c" : line 27)
                                   ( "test2.c" : line 27 , 12 )
                                   Name { nameId = 97 })))
                          (NodeInfo
                             ("test2.c" : line 27)
                             ( "test2.c" : line 27 , 12 )
                             Name { nameId = 98 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 27)
                    ( "test2.c" : line 27 , 1 )
                    Name { nameId = 99 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 28)
                             ( "test2.c" : line 28 , 1 )
                             Name { nameId = 100 }))
                       (NodeInfo
                          ("test2.c" : line 28)
                          ( "test2.c" : line 28 , 1 )
                          Name { nameId = 101 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 28)
                             ( "test2.c" : line 28 , 2 )
                             Name { nameId = 102 })))
                    (NodeInfo
                       ("test2.c" : line 28)
                       ( "test2.c" : line 28 , 2 )
                       Name { nameId = 103 }))
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
                                          ("test2.c" : line 30)
                                          ( "test2.c" : line 30 , 1 )
                                          Name { nameId = 104 }))
                                    (NodeInfo
                                       ("test2.c" : line 30)
                                       ( "test2.c" : line 30 , 1 )
                                       Name { nameId = 105 }))
                                 (NodeInfo
                                    ("test2.c" : line 30)
                                    ( "test2.c" : line 30 , 2 )
                                    Name { nameId = 106 })))
                           (NodeInfo
                              ("test2.c" : line 30)
                              ( "test2.c" : line 30 , 1 )
                              Name { nameId = 107 }))
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
                                             ("test2.c" : line 31)
                                             ( "test2.c" : line 31 , 1 )
                                             Name { nameId = 108 }))
                                       (NodeInfo
                                          ("test2.c" : line 31)
                                          ( "test2.c" : line 31 , 1 )
                                          Name { nameId = 109 }))
                                    (NodeInfo
                                       ("test2.c" : line 31)
                                       ( "test2.c" : line 31 , 1 )
                                       Name { nameId = 110 }))
                                 (CConst
                                    (CStrConst
                                       "jkl;"
                                       (NodeInfo
                                          ("test2.c" : line 31)
                                          ( "test2.c" : line 31 , 6 )
                                          Name { nameId = 111 })))
                                 (NodeInfo
                                    ("test2.c" : line 31)
                                    ( "test2.c" : line 31 , 6 )
                                    Name { nameId = 112 })))
                           (NodeInfo
                              ("test2.c" : line 31)
                              ( "test2.c" : line 31 , 1 )
                              Name { nameId = 113 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 29)
                       ( "test2.c" : line 32 , 1 )
                       Name { nameId = 114 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 28)
                    ( "test2.c" : line 32 , 1 )
                    Name { nameId = 115 }))
          ]
          (NodeInfo
             ("test2.c" : line 6)
             ( "test2.c" : line 33 , 1 )
             Name { nameId = 116 }))
       (NodeInfo
          ("test2.c" : line 5)
          ( "test2.c" : line 33 , 1 )
          Name { nameId = 117 }))
]