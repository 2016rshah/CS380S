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
           (CCharType
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 4 )
                 Name { nameId = 12 }))
       ]
       (CDeclr
          (Just
             (Ident
                "g"
                103
                (NodeInfo
                   ("test2.c" : line 5)
                   ( "test2.c" : line 5 , 1 )
                   Name { nameId = 13 })))
          [ CFunDeclr
              (Right ( [] , False ))
              []
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 1 )
                 Name { nameId = 15 })
          , CPtrDeclr
              []
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 1 )
                 Name { nameId = 16 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 5)
             ( "test2.c" : line 5 , 1 )
             Name { nameId = 14 }))
       []
       (CCompound
          []
          [ CBlockStmt
              (CReturn
                 (Just
                    (CConst
                       (CStrConst
                          "PRESERVING_RETURN"
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 19 )
                             Name { nameId = 17 }))))
                 (NodeInfo
                    ("test2.c" : line 7)
                    ( "test2.c" : line 7 , 1 )
                    Name { nameId = 18 }))
          ]
          (NodeInfo
             ("test2.c" : line 6)
             ( "test2.c" : line 8 , 1 )
             Name { nameId = 19 }))
       (NodeInfo
          ("test2.c" : line 5)
          ( "test2.c" : line 8 , 1 )
          Name { nameId = 20 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test2.c" : line 9)
                 ( "test2.c" : line 9 , 3 )
                 Name { nameId = 22 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test2.c" : line 9)
                   ( "test2.c" : line 9 , 4 )
                   Name { nameId = 21 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 9)
                                 ( "test2.c" : line 9 , 3 )
                                 Name { nameId = 25 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argc"
                                      209320289
                                      (NodeInfo
                                         ("test2.c" : line 9)
                                         ( "test2.c" : line 9 , 4 )
                                         Name { nameId = 24 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 4 )
                                   Name { nameId = 26 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 9)
                          ( "test2.c" : line 9 , 4 )
                          Name { nameId = 27 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test2.c" : line 9)
                                 ( "test2.c" : line 9 , 4 )
                                 Name { nameId = 28 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argv"
                                      249166177
                                      (NodeInfo
                                         ("test2.c" : line 9)
                                         ( "test2.c" : line 9 , 4 )
                                         Name { nameId = 29 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 9)
                                       ( "test2.c" : line 9 , 4 )
                                       Name { nameId = 31 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 9)
                                       ( "test2.c" : line 9 , 4 )
                                       Name { nameId = 32 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 4 )
                                   Name { nameId = 30 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 9)
                          ( "test2.c" : line 9 , 4 )
                          Name { nameId = 33 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 9)
                 ( "test2.c" : line 9 , 1 )
                 Name { nameId = 34 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 9)
             ( "test2.c" : line 9 , 4 )
             Name { nameId = 23 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 11)
                           ( "test2.c" : line 11 , 4 )
                           Name { nameId = 35 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 11)
                                   ( "test2.c" : line 11 , 1 )
                                   Name { nameId = 36 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 11)
                                 ( "test2.c" : line 11 , 1 )
                                 Name { nameId = 38 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 1 )
                             Name { nameId = 37 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "asdf"
                                (NodeInfo
                                   ("test2.c" : line 11)
                                   ( "test2.c" : line 11 , 6 )
                                   Name { nameId = 39 })))
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 6 )
                             Name { nameId = 40 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 11)
                    ( "test2.c" : line 11 , 1 )
                    Name { nameId = 41 }))
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
                                   ("test2.c" : line 12)
                                   ( "test2.c" : line 12 , 1 )
                                   Name { nameId = 42 }))
                             (NodeInfo
                                ("test2.c" : line 12)
                                ( "test2.c" : line 12 , 1 )
                                Name { nameId = 43 }))
                          (NodeInfo
                             ("test2.c" : line 12)
                             ( "test2.c" : line 12 , 1 )
                             Name { nameId = 44 }))
                       (CConst
                          (CStrConst
                             "zxcv"
                             (NodeInfo
                                ("test2.c" : line 12)
                                ( "test2.c" : line 12 , 6 )
                                Name { nameId = 45 })))
                       (NodeInfo
                          ("test2.c" : line 12)
                          ( "test2.c" : line 12 , 6 )
                          Name { nameId = 46 })))
                 (NodeInfo
                    ("test2.c" : line 12)
                    ( "test2.c" : line 12 , 1 )
                    Name { nameId = 47 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 13)
                           ( "test2.c" : line 13 , 4 )
                           Name { nameId = 48 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "foo"
                                1832934
                                (NodeInfo
                                   ("test2.c" : line 13)
                                   ( "test2.c" : line 13 , 3 )
                                   Name { nameId = 49 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 13)
                                 ( "test2.c" : line 13 , 3 )
                                 Name { nameId = 51 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 3 )
                             Name { nameId = 50 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "SOURCE"
                                (NodeInfo
                                   ("test2.c" : line 13)
                                   ( "test2.c" : line 13 , 8 )
                                   Name { nameId = 52 })))
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 8 )
                             Name { nameId = 53 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 13)
                    ( "test2.c" : line 13 , 1 )
                    Name { nameId = 54 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 14)
                           ( "test2.c" : line 14 , 4 )
                           Name { nameId = 55 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "bar"
                                1880290
                                (NodeInfo
                                   ("test2.c" : line 14)
                                   ( "test2.c" : line 14 , 3 )
                                   Name { nameId = 56 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 14)
                                 ( "test2.c" : line 14 , 3 )
                                 Name { nameId = 58 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 3 )
                             Name { nameId = 57 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "SOURCE"
                                (NodeInfo
                                   ("test2.c" : line 14)
                                   ( "test2.c" : line 14 , 8 )
                                   Name { nameId = 59 })))
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 8 )
                             Name { nameId = 60 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 14)
                    ( "test2.c" : line 14 , 1 )
                    Name { nameId = 61 }))
          , CBlockStmt
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
                             Name { nameId = 62 }))
                       (NodeInfo
                          ("test2.c" : line 15)
                          ( "test2.c" : line 15 , 1 )
                          Name { nameId = 63 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 15)
                             ( "test2.c" : line 15 , 6 )
                             Name { nameId = 64 })))
                    (NodeInfo
                       ("test2.c" : line 15)
                       ( "test2.c" : line 15 , 6 )
                       Name { nameId = 65 }))
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
                                             ("test2.c" : line 17)
                                             ( "test2.c" : line 17 , 1 )
                                             Name { nameId = 66 }))
                                       (NodeInfo
                                          ("test2.c" : line 17)
                                          ( "test2.c" : line 17 , 1 )
                                          Name { nameId = 67 }))
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 1 )
                                       Name { nameId = 68 }))
                                 (CConst
                                    (CStrConst
                                       "PRESERVING"
                                       (NodeInfo
                                          ("test2.c" : line 17)
                                          ( "test2.c" : line 17 , 12 )
                                          Name { nameId = 69 })))
                                 (NodeInfo
                                    ("test2.c" : line 17)
                                    ( "test2.c" : line 17 , 12 )
                                    Name { nameId = 70 })))
                           (NodeInfo
                              ("test2.c" : line 17)
                              ( "test2.c" : line 17 , 1 )
                              Name { nameId = 71 }))
                    , CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 18)
                                       ( "test2.c" : line 18 , 1 )
                                       Name { nameId = 72 }))))
                           (NodeInfo
                              ("test2.c" : line 18)
                              ( "test2.c" : line 18 , 1 )
                              Name { nameId = 73 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 16)
                       ( "test2.c" : line 19 , 1 )
                       Name { nameId = 74 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 20)
                                   ( "test2.c" : line 20 , 1 )
                                   Name { nameId = 75 }))
                             (NodeInfo
                                ("test2.c" : line 20)
                                ( "test2.c" : line 20 , 1 )
                                Name { nameId = 76 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 20)
                                   ( "test2.c" : line 20 , 6 )
                                   Name { nameId = 77 })))
                          (NodeInfo
                             ("test2.c" : line 20)
                             ( "test2.c" : line 20 , 6 )
                             Name { nameId = 78 }))
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
                                                "bar"
                                                1880290
                                                (NodeInfo
                                                   ("test2.c" : line 22)
                                                   ( "test2.c" : line 22 , 3 )
                                                   Name { nameId = 79 }))
                                             (NodeInfo
                                                ("test2.c" : line 22)
                                                ( "test2.c" : line 22 , 3 )
                                                Name { nameId = 80 }))
                                          (NodeInfo
                                             ("test2.c" : line 22)
                                             ( "test2.c" : line 22 , 3 )
                                             Name { nameId = 81 }))
                                       (CConst
                                          (CStrConst
                                             "PRESERVING"
                                             (NodeInfo
                                                ("test2.c" : line 22)
                                                ( "test2.c" : line 22 , 12 )
                                                Name { nameId = 82 })))
                                       (NodeInfo
                                          ("test2.c" : line 22)
                                          ( "test2.c" : line 22 , 12 )
                                          Name { nameId = 83 })))
                                 (NodeInfo
                                    ("test2.c" : line 22)
                                    ( "test2.c" : line 22 , 1 )
                                    Name { nameId = 84 }))
                          , CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 23)
                                             ( "test2.c" : line 23 , 1 )
                                             Name { nameId = 85 }))))
                                 (NodeInfo
                                    ("test2.c" : line 23)
                                    ( "test2.c" : line 23 , 1 )
                                    Name { nameId = 86 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 21)
                             ( "test2.c" : line 24 , 1 )
                             Name { nameId = 87 }))
                       (Just
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
                                                   "foo"
                                                   1832934
                                                   (NodeInfo
                                                      ("test2.c" : line 27)
                                                      ( "test2.c" : line 27 , 3 )
                                                      Name { nameId = 88 }))
                                                (NodeInfo
                                                   ("test2.c" : line 27)
                                                   ( "test2.c" : line 27 , 3 )
                                                   Name { nameId = 89 }))
                                             (NodeInfo
                                                ("test2.c" : line 27)
                                                ( "test2.c" : line 27 , 3 )
                                                Name { nameId = 90 }))
                                          (CConst
                                             (CStrConst
                                                "PRESERVING"
                                                (NodeInfo
                                                   ("test2.c" : line 27)
                                                   ( "test2.c" : line 27 , 12 )
                                                   Name { nameId = 91 })))
                                          (NodeInfo
                                             ("test2.c" : line 27)
                                             ( "test2.c" : line 27 , 12 )
                                             Name { nameId = 92 })))
                                    (NodeInfo
                                       ("test2.c" : line 27)
                                       ( "test2.c" : line 27 , 1 )
                                       Name { nameId = 93 }))
                             , CBlockStmt
                                 (CReturn
                                    (Just
                                       (CConst
                                          (CIntConst
                                             2
                                             (NodeInfo
                                                ("test2.c" : line 28)
                                                ( "test2.c" : line 28 , 1 )
                                                Name { nameId = 94 }))))
                                    (NodeInfo
                                       ("test2.c" : line 28)
                                       ( "test2.c" : line 28 , 1 )
                                       Name { nameId = 95 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 26)
                                ( "test2.c" : line 29 , 1 )
                                Name { nameId = 96 })))
                       (NodeInfo
                          ("test2.c" : line 20)
                          ( "test2.c" : line 29 , 1 )
                          Name { nameId = 97 })))
                 (NodeInfo
                    ("test2.c" : line 15)
                    ( "test2.c" : line 29 , 1 )
                    Name { nameId = 98 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test2.c" : line 30)
                                 ( "test2.c" : line 30 , 4 )
                                 Name { nameId = 99 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 30)
                                         ( "test2.c" : line 30 , 1 )
                                         Name { nameId = 100 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 30)
                                       ( "test2.c" : line 30 , 1 )
                                       Name { nameId = 102 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 30)
                                   ( "test2.c" : line 30 , 1 )
                                   Name { nameId = 101 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CStrConst
                                      "PRESERVING"
                                      (NodeInfo
                                         ("test2.c" : line 30)
                                         ( "test2.c" : line 30 , 12 )
                                         Name { nameId = 103 })))
                                (NodeInfo
                                   ("test2.c" : line 30)
                                   ( "test2.c" : line 30 , 12 )
                                   Name { nameId = 104 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 30)
                          ( "test2.c" : line 30 , 1 )
                          Name { nameId = 105 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 1 )
                                Name { nameId = 106 }))
                          (NodeInfo
                             ("test2.c" : line 30)
                             ( "test2.c" : line 30 , 1 )
                             Name { nameId = 107 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 2 )
                                Name { nameId = 108 })))
                       (NodeInfo
                          ("test2.c" : line 30)
                          ( "test2.c" : line 30 , 2 )
                          Name { nameId = 109 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 1 )
                                Name { nameId = 110 }))
                          (NodeInfo
                             ("test2.c" : line 30)
                             ( "test2.c" : line 30 , 1 )
                             Name { nameId = 111 }))
                       (NodeInfo
                          ("test2.c" : line 30)
                          ( "test2.c" : line 30 , 2 )
                          Name { nameId = 112 })))
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
                                             ("test2.c" : line 32)
                                             ( "test2.c" : line 32 , 1 )
                                             Name { nameId = 113 }))
                                       (NodeInfo
                                          ("test2.c" : line 32)
                                          ( "test2.c" : line 32 , 1 )
                                          Name { nameId = 114 }))
                                    (NodeInfo
                                       ("test2.c" : line 32)
                                       ( "test2.c" : line 32 , 1 )
                                       Name { nameId = 115 }))
                                 (CConst
                                    (CStrConst
                                       "PRESERVING"
                                       (NodeInfo
                                          ("test2.c" : line 32)
                                          ( "test2.c" : line 32 , 12 )
                                          Name { nameId = 116 })))
                                 (NodeInfo
                                    ("test2.c" : line 32)
                                    ( "test2.c" : line 32 , 12 )
                                    Name { nameId = 117 })))
                           (NodeInfo
                              ("test2.c" : line 32)
                              ( "test2.c" : line 32 , 1 )
                              Name { nameId = 118 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 31)
                       ( "test2.c" : line 33 , 1 )
                       Name { nameId = 119 }))
                 (NodeInfo
                    ("test2.c" : line 30)
                    ( "test2.c" : line 33 , 1 )
                    Name { nameId = 120 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 34)
                           ( "test2.c" : line 34 , 4 )
                           Name { nameId = 121 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 34)
                                   ( "test2.c" : line 34 , 1 )
                                   Name { nameId = 122 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 34)
                                 ( "test2.c" : line 34 , 1 )
                                 Name { nameId = 124 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 34)
                             ( "test2.c" : line 34 , 1 )
                             Name { nameId = 123 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "PRESERVING"
                                (NodeInfo
                                   ("test2.c" : line 34)
                                   ( "test2.c" : line 34 , 12 )
                                   Name { nameId = 125 })))
                          (NodeInfo
                             ("test2.c" : line 34)
                             ( "test2.c" : line 34 , 12 )
                             Name { nameId = 126 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 34)
                    ( "test2.c" : line 34 , 1 )
                    Name { nameId = 127 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CUnary
                          CIndOp
                          (CVar
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 35)
                                   ( "test2.c" : line 35 , 1 )
                                   Name { nameId = 128 }))
                             (NodeInfo
                                ("test2.c" : line 35)
                                ( "test2.c" : line 35 , 1 )
                                Name { nameId = 129 }))
                          (NodeInfo
                             ("test2.c" : line 35)
                             ( "test2.c" : line 35 , 1 )
                             Name { nameId = 130 }))
                       (CConst
                          (CStrConst
                             "PRESERVING"
                             (NodeInfo
                                ("test2.c" : line 35)
                                ( "test2.c" : line 35 , 12 )
                                Name { nameId = 131 })))
                       (NodeInfo
                          ("test2.c" : line 35)
                          ( "test2.c" : line 35 , 12 )
                          Name { nameId = 132 })))
                 (NodeInfo
                    ("test2.c" : line 35)
                    ( "test2.c" : line 35 , 1 )
                    Name { nameId = 133 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 36)
                             ( "test2.c" : line 36 , 1 )
                             Name { nameId = 134 }))
                       (NodeInfo
                          ("test2.c" : line 36)
                          ( "test2.c" : line 36 , 1 )
                          Name { nameId = 135 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 36)
                             ( "test2.c" : line 36 , 2 )
                             Name { nameId = 136 })))
                    (NodeInfo
                       ("test2.c" : line 36)
                       ( "test2.c" : line 36 , 2 )
                       Name { nameId = 137 }))
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
                                          ("test2.c" : line 38)
                                          ( "test2.c" : line 38 , 1 )
                                          Name { nameId = 138 }))
                                    (NodeInfo
                                       ("test2.c" : line 38)
                                       ( "test2.c" : line 38 , 1 )
                                       Name { nameId = 139 }))
                                 (NodeInfo
                                    ("test2.c" : line 38)
                                    ( "test2.c" : line 38 , 2 )
                                    Name { nameId = 140 })))
                           (NodeInfo
                              ("test2.c" : line 38)
                              ( "test2.c" : line 38 , 1 )
                              Name { nameId = 141 }))
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
                                             ("test2.c" : line 39)
                                             ( "test2.c" : line 39 , 1 )
                                             Name { nameId = 142 }))
                                       (NodeInfo
                                          ("test2.c" : line 39)
                                          ( "test2.c" : line 39 , 1 )
                                          Name { nameId = 143 }))
                                    (NodeInfo
                                       ("test2.c" : line 39)
                                       ( "test2.c" : line 39 , 1 )
                                       Name { nameId = 144 }))
                                 (CConst
                                    (CStrConst
                                       "PRESERVING"
                                       (NodeInfo
                                          ("test2.c" : line 39)
                                          ( "test2.c" : line 39 , 12 )
                                          Name { nameId = 145 })))
                                 (NodeInfo
                                    ("test2.c" : line 39)
                                    ( "test2.c" : line 39 , 12 )
                                    Name { nameId = 146 })))
                           (NodeInfo
                              ("test2.c" : line 39)
                              ( "test2.c" : line 39 , 1 )
                              Name { nameId = 147 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 37)
                       ( "test2.c" : line 40 , 1 )
                       Name { nameId = 148 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 36)
                    ( "test2.c" : line 40 , 1 )
                    Name { nameId = 149 }))
          ]
          (NodeInfo
             ("test2.c" : line 10)
             ( "test2.c" : line 41 , 1 )
             Name { nameId = 150 }))
       (NodeInfo
          ("test2.c" : line 9)
          ( "test2.c" : line 41 , 1 )
          Name { nameId = 151 }))
]