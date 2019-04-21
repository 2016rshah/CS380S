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
                          "preservational"
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 16 )
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
                          , CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 13)
                                 ( "test2.c" : line 13 , 3 )
                                 Name { nameId = 52 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 3 )
                             Name { nameId = 50 }))
                   , Just
                       (CInitExpr
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 13)
                                   ( "test2.c" : line 13 , 1 )
                                   Name { nameId = 53 }))
                             (NodeInfo
                                ("test2.c" : line 13)
                                ( "test2.c" : line 13 , 1 )
                                Name { nameId = 54 }))
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 1 )
                             Name { nameId = 55 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 13)
                    ( "test2.c" : line 13 , 1 )
                    Name { nameId = 56 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 14)
                           ( "test2.c" : line 14 , 4 )
                           Name { nameId = 57 }))
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
                                   Name { nameId = 58 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 14)
                                 ( "test2.c" : line 14 , 3 )
                                 Name { nameId = 60 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 3 )
                             Name { nameId = 59 }))
                   , Just
                       (CInitExpr
                          (CUnary
                             CIndOp
                             (CVar
                                (Ident
                                   "t"
                                   116
                                   (NodeInfo
                                      ("test2.c" : line 14)
                                      ( "test2.c" : line 14 , 1 )
                                      Name { nameId = 61 }))
                                (NodeInfo
                                   ("test2.c" : line 14)
                                   ( "test2.c" : line 14 , 1 )
                                   Name { nameId = 62 }))
                             (NodeInfo
                                ("test2.c" : line 14)
                                ( "test2.c" : line 14 , 1 )
                                Name { nameId = 63 }))
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 1 )
                             Name { nameId = 64 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 14)
                    ( "test2.c" : line 14 , 1 )
                    Name { nameId = 65 }))
          , CBlockStmt
              (CFor
                 (Left Nothing)
                 Nothing
                 Nothing
                 (CCompound
                    []
                    [ CBlockDecl
                        (CDecl
                           [ CTypeSpec
                               (CCharType
                                  (NodeInfo
                                     ("test2.c" : line 17)
                                     ( "test2.c" : line 17 , 4 )
                                     Name { nameId = 66 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 17)
                                             ( "test2.c" : line 17 , 3 )
                                             Name { nameId = 67 })))
                                    [ CPtrDeclr
                                        []
                                        (NodeInfo
                                           ("test2.c" : line 17)
                                           ( "test2.c" : line 17 , 3 )
                                           Name { nameId = 69 })
                                    ]
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 3 )
                                       Name { nameId = 68 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CStrConst
                                          "SOURCE"
                                          (NodeInfo
                                             ("test2.c" : line 17)
                                             ( "test2.c" : line 17 , 8 )
                                             Name { nameId = 70 })))
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 8 )
                                       Name { nameId = 71 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 17)
                              ( "test2.c" : line 17 , 1 )
                              Name { nameId = 72 }))
                    , CBlockDecl
                        (CDecl
                           [ CTypeSpec
                               (CCharType
                                  (NodeInfo
                                     ("test2.c" : line 18)
                                     ( "test2.c" : line 18 , 4 )
                                     Name { nameId = 73 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 18)
                                             ( "test2.c" : line 18 , 4 )
                                             Name { nameId = 74 })))
                                    [ CPtrDeclr
                                        []
                                        (NodeInfo
                                           ("test2.c" : line 18)
                                           ( "test2.c" : line 18 , 4 )
                                           Name { nameId = 76 })
                                    ]
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 18)
                                       ( "test2.c" : line 18 , 4 )
                                       Name { nameId = 75 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CStrConst
                                          "HIGH ENTROPY"
                                          (NodeInfo
                                             ("test2.c" : line 18)
                                             ( "test2.c" : line 18 , 14 )
                                             Name { nameId = 77 })))
                                    (NodeInfo
                                       ("test2.c" : line 18)
                                       ( "test2.c" : line 18 , 14 )
                                       Name { nameId = 78 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 18)
                              ( "test2.c" : line 18 , 1 )
                              Name { nameId = 79 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAssignOp
                                 (CUnary
                                    CIndOp
                                    (CVar
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 19)
                                             ( "test2.c" : line 19 , 4 )
                                             Name { nameId = 80 }))
                                       (NodeInfo
                                          ("test2.c" : line 19)
                                          ( "test2.c" : line 19 , 4 )
                                          Name { nameId = 81 }))
                                    (NodeInfo
                                       ("test2.c" : line 19)
                                       ( "test2.c" : line 19 , 4 )
                                       Name { nameId = 82 }))
                                 (CConst
                                    (CStrConst
                                       "HIGH ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 19)
                                          ( "test2.c" : line 19 , 14 )
                                          Name { nameId = 83 })))
                                 (NodeInfo
                                    ("test2.c" : line 19)
                                    ( "test2.c" : line 19 , 14 )
                                    Name { nameId = 84 })))
                           (NodeInfo
                              ("test2.c" : line 19)
                              ( "test2.c" : line 19 , 1 )
                              Name { nameId = 85 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAndAssOp
                                 (CUnary
                                    CIndOp
                                    (CVar
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 20)
                                             ( "test2.c" : line 20 , 4 )
                                             Name { nameId = 86 }))
                                       (NodeInfo
                                          ("test2.c" : line 20)
                                          ( "test2.c" : line 20 , 4 )
                                          Name { nameId = 87 }))
                                    (NodeInfo
                                       ("test2.c" : line 20)
                                       ( "test2.c" : line 20 , 4 )
                                       Name { nameId = 88 }))
                                 (CConst
                                    (CStrConst
                                       "LOW ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 20)
                                          ( "test2.c" : line 20 , 13 )
                                          Name { nameId = 89 })))
                                 (NodeInfo
                                    ("test2.c" : line 20)
                                    ( "test2.c" : line 20 , 13 )
                                    Name { nameId = 90 })))
                           (NodeInfo
                              ("test2.c" : line 20)
                              ( "test2.c" : line 20 , 1 )
                              Name { nameId = 91 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAssignOp
                                 (CUnary
                                    CIndOp
                                    (CVar
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 4 )
                                             Name { nameId = 92 }))
                                       (NodeInfo
                                          ("test2.c" : line 21)
                                          ( "test2.c" : line 21 , 4 )
                                          Name { nameId = 93 }))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 4 )
                                       Name { nameId = 94 }))
                                 (CBinary
                                    CAddOp
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 1 )
                                             Name { nameId = 95 })))
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 1 )
                                             Name { nameId = 96 })))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 1 )
                                       Name { nameId = 97 }))
                                 (NodeInfo
                                    ("test2.c" : line 21)
                                    ( "test2.c" : line 21 , 1 )
                                    Name { nameId = 98 })))
                           (NodeInfo
                              ("test2.c" : line 21)
                              ( "test2.c" : line 21 , 1 )
                              Name { nameId = 99 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 16)
                       ( "test2.c" : line 22 , 1 )
                       Name { nameId = 100 }))
                 (NodeInfo
                    ("test2.c" : line 15)
                    ( "test2.c" : line 22 , 1 )
                    Name { nameId = 101 }))
          , CBlockStmt
              (CIf
                 (CBinary
                    CEqOp
                    (CVar
                       (Ident
                          "t"
                          116
                          (NodeInfo
                             ("test2.c" : line 23)
                             ( "test2.c" : line 23 , 1 )
                             Name { nameId = 102 }))
                       (NodeInfo
                          ("test2.c" : line 23)
                          ( "test2.c" : line 23 , 1 )
                          Name { nameId = 103 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 23)
                             ( "test2.c" : line 23 , 6 )
                             Name { nameId = 104 })))
                    (NodeInfo
                       ("test2.c" : line 23)
                       ( "test2.c" : line 23 , 6 )
                       Name { nameId = 105 }))
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
                                             Name { nameId = 106 }))
                                       (NodeInfo
                                          ("test2.c" : line 25)
                                          ( "test2.c" : line 25 , 1 )
                                          Name { nameId = 107 }))
                                    (NodeInfo
                                       ("test2.c" : line 25)
                                       ( "test2.c" : line 25 , 1 )
                                       Name { nameId = 108 }))
                                 (CConst
                                    (CStrConst
                                       "1234"
                                       (NodeInfo
                                          ("test2.c" : line 25)
                                          ( "test2.c" : line 25 , 6 )
                                          Name { nameId = 109 })))
                                 (NodeInfo
                                    ("test2.c" : line 25)
                                    ( "test2.c" : line 25 , 6 )
                                    Name { nameId = 110 })))
                           (NodeInfo
                              ("test2.c" : line 25)
                              ( "test2.c" : line 25 , 1 )
                              Name { nameId = 111 }))
                    , CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 26)
                                       ( "test2.c" : line 26 , 1 )
                                       Name { nameId = 112 }))))
                           (NodeInfo
                              ("test2.c" : line 26)
                              ( "test2.c" : line 26 , 1 )
                              Name { nameId = 113 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 24)
                       ( "test2.c" : line 27 , 1 )
                       Name { nameId = 114 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 28)
                                   ( "test2.c" : line 28 , 1 )
                                   Name { nameId = 115 }))
                             (NodeInfo
                                ("test2.c" : line 28)
                                ( "test2.c" : line 28 , 1 )
                                Name { nameId = 116 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 28)
                                   ( "test2.c" : line 28 , 6 )
                                   Name { nameId = 117 })))
                          (NodeInfo
                             ("test2.c" : line 28)
                             ( "test2.c" : line 28 , 6 )
                             Name { nameId = 118 }))
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
                                                   ("test2.c" : line 30)
                                                   ( "test2.c" : line 30 , 3 )
                                                   Name { nameId = 119 }))
                                             (NodeInfo
                                                ("test2.c" : line 30)
                                                ( "test2.c" : line 30 , 3 )
                                                Name { nameId = 120 }))
                                          (NodeInfo
                                             ("test2.c" : line 30)
                                             ( "test2.c" : line 30 , 3 )
                                             Name { nameId = 121 }))
                                       (CConst
                                          (CStrConst
                                             "dfgh"
                                             (NodeInfo
                                                ("test2.c" : line 30)
                                                ( "test2.c" : line 30 , 6 )
                                                Name { nameId = 122 })))
                                       (NodeInfo
                                          ("test2.c" : line 30)
                                          ( "test2.c" : line 30 , 6 )
                                          Name { nameId = 123 })))
                                 (NodeInfo
                                    ("test2.c" : line 30)
                                    ( "test2.c" : line 30 , 1 )
                                    Name { nameId = 124 }))
                          , CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 31)
                                             ( "test2.c" : line 31 , 1 )
                                             Name { nameId = 125 }))))
                                 (NodeInfo
                                    ("test2.c" : line 31)
                                    ( "test2.c" : line 31 , 1 )
                                    Name { nameId = 126 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 29)
                             ( "test2.c" : line 32 , 1 )
                             Name { nameId = 127 }))
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
                                                      ("test2.c" : line 35)
                                                      ( "test2.c" : line 35 , 3 )
                                                      Name { nameId = 128 }))
                                                (NodeInfo
                                                   ("test2.c" : line 35)
                                                   ( "test2.c" : line 35 , 3 )
                                                   Name { nameId = 129 }))
                                             (NodeInfo
                                                ("test2.c" : line 35)
                                                ( "test2.c" : line 35 , 3 )
                                                Name { nameId = 130 }))
                                          (CConst
                                             (CStrConst
                                                "xcvvcbn"
                                                (NodeInfo
                                                   ("test2.c" : line 35)
                                                   ( "test2.c" : line 35 , 9 )
                                                   Name { nameId = 131 })))
                                          (NodeInfo
                                             ("test2.c" : line 35)
                                             ( "test2.c" : line 35 , 9 )
                                             Name { nameId = 132 })))
                                    (NodeInfo
                                       ("test2.c" : line 35)
                                       ( "test2.c" : line 35 , 1 )
                                       Name { nameId = 133 }))
                             , CBlockStmt
                                 (CReturn
                                    (Just
                                       (CConst
                                          (CIntConst
                                             2
                                             (NodeInfo
                                                ("test2.c" : line 36)
                                                ( "test2.c" : line 36 , 1 )
                                                Name { nameId = 134 }))))
                                    (NodeInfo
                                       ("test2.c" : line 36)
                                       ( "test2.c" : line 36 , 1 )
                                       Name { nameId = 135 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 34)
                                ( "test2.c" : line 37 , 1 )
                                Name { nameId = 136 })))
                       (NodeInfo
                          ("test2.c" : line 28)
                          ( "test2.c" : line 37 , 1 )
                          Name { nameId = 137 })))
                 (NodeInfo
                    ("test2.c" : line 23)
                    ( "test2.c" : line 37 , 1 )
                    Name { nameId = 138 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 38)
                                 ( "test2.c" : line 38 , 3 )
                                 Name { nameId = 140 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 38)
                                         ( "test2.c" : line 38 , 1 )
                                         Name { nameId = 139 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 38)
                                   ( "test2.c" : line 38 , 1 )
                                   Name { nameId = 141 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CIntConst
                                      0
                                      (NodeInfo
                                         ("test2.c" : line 38)
                                         ( "test2.c" : line 38 , 1 )
                                         Name { nameId = 142 })))
                                (NodeInfo
                                   ("test2.c" : line 38)
                                   ( "test2.c" : line 38 , 1 )
                                   Name { nameId = 143 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 38)
                          ( "test2.c" : line 38 , 1 )
                          Name { nameId = 144 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 38)
                                ( "test2.c" : line 38 , 1 )
                                Name { nameId = 145 }))
                          (NodeInfo
                             ("test2.c" : line 38)
                             ( "test2.c" : line 38 , 1 )
                             Name { nameId = 146 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 38)
                                ( "test2.c" : line 38 , 2 )
                                Name { nameId = 147 })))
                       (NodeInfo
                          ("test2.c" : line 38)
                          ( "test2.c" : line 38 , 2 )
                          Name { nameId = 148 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 38)
                                ( "test2.c" : line 38 , 1 )
                                Name { nameId = 149 }))
                          (NodeInfo
                             ("test2.c" : line 38)
                             ( "test2.c" : line 38 , 1 )
                             Name { nameId = 150 }))
                       (NodeInfo
                          ("test2.c" : line 38)
                          ( "test2.c" : line 38 , 2 )
                          Name { nameId = 151 })))
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
                                             ("test2.c" : line 40)
                                             ( "test2.c" : line 40 , 1 )
                                             Name { nameId = 152 }))
                                       (NodeInfo
                                          ("test2.c" : line 40)
                                          ( "test2.c" : line 40 , 1 )
                                          Name { nameId = 153 }))
                                    (NodeInfo
                                       ("test2.c" : line 40)
                                       ( "test2.c" : line 40 , 1 )
                                       Name { nameId = 154 }))
                                 (CConst
                                    (CStrConst
                                       "qwer"
                                       (NodeInfo
                                          ("test2.c" : line 40)
                                          ( "test2.c" : line 40 , 6 )
                                          Name { nameId = 155 })))
                                 (NodeInfo
                                    ("test2.c" : line 40)
                                    ( "test2.c" : line 40 , 6 )
                                    Name { nameId = 156 })))
                           (NodeInfo
                              ("test2.c" : line 40)
                              ( "test2.c" : line 40 , 1 )
                              Name { nameId = 157 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 39)
                       ( "test2.c" : line 41 , 1 )
                       Name { nameId = 158 }))
                 (NodeInfo
                    ("test2.c" : line 38)
                    ( "test2.c" : line 41 , 1 )
                    Name { nameId = 159 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 42)
                           ( "test2.c" : line 42 , 3 )
                           Name { nameId = 161 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 42)
                                   ( "test2.c" : line 42 , 1 )
                                   Name { nameId = 160 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 42)
                             ( "test2.c" : line 42 , 1 )
                             Name { nameId = 162 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 42)
                                   ( "test2.c" : line 42 , 1 )
                                   Name { nameId = 163 })))
                          (NodeInfo
                             ("test2.c" : line 42)
                             ( "test2.c" : line 42 , 1 )
                             Name { nameId = 164 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 42)
                    ( "test2.c" : line 42 , 1 )
                    Name { nameId = 165 }))
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
                                   ("test2.c" : line 43)
                                   ( "test2.c" : line 43 , 1 )
                                   Name { nameId = 166 }))
                             (NodeInfo
                                ("test2.c" : line 43)
                                ( "test2.c" : line 43 , 1 )
                                Name { nameId = 167 }))
                          (NodeInfo
                             ("test2.c" : line 43)
                             ( "test2.c" : line 43 , 1 )
                             Name { nameId = 168 }))
                       (CConst
                          (CIntConst
                             4
                             (NodeInfo
                                ("test2.c" : line 43)
                                ( "test2.c" : line 43 , 1 )
                                Name { nameId = 169 })))
                       (NodeInfo
                          ("test2.c" : line 43)
                          ( "test2.c" : line 43 , 1 )
                          Name { nameId = 170 })))
                 (NodeInfo
                    ("test2.c" : line 43)
                    ( "test2.c" : line 43 , 1 )
                    Name { nameId = 171 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 44)
                             ( "test2.c" : line 44 , 1 )
                             Name { nameId = 172 }))
                       (NodeInfo
                          ("test2.c" : line 44)
                          ( "test2.c" : line 44 , 1 )
                          Name { nameId = 173 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 44)
                             ( "test2.c" : line 44 , 2 )
                             Name { nameId = 174 })))
                    (NodeInfo
                       ("test2.c" : line 44)
                       ( "test2.c" : line 44 , 2 )
                       Name { nameId = 175 }))
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
                                          ("test2.c" : line 46)
                                          ( "test2.c" : line 46 , 1 )
                                          Name { nameId = 176 }))
                                    (NodeInfo
                                       ("test2.c" : line 46)
                                       ( "test2.c" : line 46 , 1 )
                                       Name { nameId = 177 }))
                                 (NodeInfo
                                    ("test2.c" : line 46)
                                    ( "test2.c" : line 46 , 2 )
                                    Name { nameId = 178 })))
                           (NodeInfo
                              ("test2.c" : line 46)
                              ( "test2.c" : line 46 , 1 )
                              Name { nameId = 179 }))
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
                                             ("test2.c" : line 47)
                                             ( "test2.c" : line 47 , 1 )
                                             Name { nameId = 180 }))
                                       (NodeInfo
                                          ("test2.c" : line 47)
                                          ( "test2.c" : line 47 , 1 )
                                          Name { nameId = 181 }))
                                    (NodeInfo
                                       ("test2.c" : line 47)
                                       ( "test2.c" : line 47 , 1 )
                                       Name { nameId = 182 }))
                                 (CConst
                                    (CStrConst
                                       "jkl;"
                                       (NodeInfo
                                          ("test2.c" : line 47)
                                          ( "test2.c" : line 47 , 6 )
                                          Name { nameId = 183 })))
                                 (NodeInfo
                                    ("test2.c" : line 47)
                                    ( "test2.c" : line 47 , 6 )
                                    Name { nameId = 184 })))
                           (NodeInfo
                              ("test2.c" : line 47)
                              ( "test2.c" : line 47 , 1 )
                              Name { nameId = 185 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 45)
                       ( "test2.c" : line 48 , 1 )
                       Name { nameId = 186 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 44)
                    ( "test2.c" : line 48 , 1 )
                    Name { nameId = 187 }))
          ]
          (NodeInfo
             ("test2.c" : line 10)
             ( "test2.c" : line 49 , 1 )
             Name { nameId = 188 }))
       (NodeInfo
          ("test2.c" : line 9)
          ( "test2.c" : line 49 , 1 )
          Name { nameId = 189 }))
]