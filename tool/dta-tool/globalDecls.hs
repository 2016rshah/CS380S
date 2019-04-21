[ CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test2.c" : line 1)
                 ( "test2.c" : line 1 , 3 )
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
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 1)
                                 ( "test2.c" : line 1 , 3 )
                                 Name { nameId = 4 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "k"
                                      107
                                      (NodeInfo
                                         ("test2.c" : line 1)
                                         ( "test2.c" : line 1 , 1 )
                                         Name { nameId = 3 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 1)
                                   ( "test2.c" : line 1 , 1 )
                                   Name { nameId = 5 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 1)
                          ( "test2.c" : line 1 , 1 )
                          Name { nameId = 6 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 1)
                 ( "test2.c" : line 1 , 1 )
                 Name { nameId = 7 })
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
          [ CBlockStmt
              (CReturn
                 (Just
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 3)
                             ( "test2.c" : line 3 , 1 )
                             Name { nameId = 8 }))
                       (NodeInfo
                          ("test2.c" : line 3)
                          ( "test2.c" : line 3 , 1 )
                          Name { nameId = 9 })))
                 (NodeInfo
                    ("test2.c" : line 3)
                    ( "test2.c" : line 3 , 1 )
                    Name { nameId = 10 }))
          ]
          (NodeInfo
             ("test2.c" : line 2)
             ( "test2.c" : line 4 , 1 )
             Name { nameId = 11 }))
       (NodeInfo
          ("test2.c" : line 1)
          ( "test2.c" : line 4 , 1 )
          Name { nameId = 12 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 4 )
                 Name { nameId = 13 }))
       ]
       (CDeclr
          (Just
             (Ident
                "g"
                103
                (NodeInfo
                   ("test2.c" : line 5)
                   ( "test2.c" : line 5 , 1 )
                   Name { nameId = 14 })))
          [ CFunDeclr
              (Right ( [] , False ))
              []
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 1 )
                 Name { nameId = 16 })
          , CPtrDeclr
              []
              (NodeInfo
                 ("test2.c" : line 5)
                 ( "test2.c" : line 5 , 1 )
                 Name { nameId = 17 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 5)
             ( "test2.c" : line 5 , 1 )
             Name { nameId = 15 }))
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
                             Name { nameId = 18 }))))
                 (NodeInfo
                    ("test2.c" : line 7)
                    ( "test2.c" : line 7 , 1 )
                    Name { nameId = 19 }))
          ]
          (NodeInfo
             ("test2.c" : line 6)
             ( "test2.c" : line 8 , 1 )
             Name { nameId = 20 }))
       (NodeInfo
          ("test2.c" : line 5)
          ( "test2.c" : line 8 , 1 )
          Name { nameId = 21 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test2.c" : line 9)
                 ( "test2.c" : line 9 , 3 )
                 Name { nameId = 23 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test2.c" : line 9)
                   ( "test2.c" : line 9 , 4 )
                   Name { nameId = 22 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 9)
                                 ( "test2.c" : line 9 , 3 )
                                 Name { nameId = 26 }))
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
                                         Name { nameId = 25 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 4 )
                                   Name { nameId = 27 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 9)
                          ( "test2.c" : line 9 , 4 )
                          Name { nameId = 28 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test2.c" : line 9)
                                 ( "test2.c" : line 9 , 4 )
                                 Name { nameId = 29 }))
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
                                         Name { nameId = 30 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 9)
                                       ( "test2.c" : line 9 , 4 )
                                       Name { nameId = 32 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 9)
                                       ( "test2.c" : line 9 , 4 )
                                       Name { nameId = 33 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 9)
                                   ( "test2.c" : line 9 , 4 )
                                   Name { nameId = 31 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 9)
                          ( "test2.c" : line 9 , 4 )
                          Name { nameId = 34 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 9)
                 ( "test2.c" : line 9 , 1 )
                 Name { nameId = 35 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 9)
             ( "test2.c" : line 9 , 4 )
             Name { nameId = 24 }))
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
                           Name { nameId = 36 }))
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
                                   Name { nameId = 37 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 11)
                                 ( "test2.c" : line 11 , 1 )
                                 Name { nameId = 39 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 1 )
                             Name { nameId = 38 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "asdf"
                                (NodeInfo
                                   ("test2.c" : line 11)
                                   ( "test2.c" : line 11 , 6 )
                                   Name { nameId = 40 })))
                          (NodeInfo
                             ("test2.c" : line 11)
                             ( "test2.c" : line 11 , 6 )
                             Name { nameId = 41 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 11)
                    ( "test2.c" : line 11 , 1 )
                    Name { nameId = 42 }))
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
                                   Name { nameId = 43 }))
                             (NodeInfo
                                ("test2.c" : line 12)
                                ( "test2.c" : line 12 , 1 )
                                Name { nameId = 44 }))
                          (NodeInfo
                             ("test2.c" : line 12)
                             ( "test2.c" : line 12 , 1 )
                             Name { nameId = 45 }))
                       (CConst
                          (CStrConst
                             "zxcv"
                             (NodeInfo
                                ("test2.c" : line 12)
                                ( "test2.c" : line 12 , 6 )
                                Name { nameId = 46 })))
                       (NodeInfo
                          ("test2.c" : line 12)
                          ( "test2.c" : line 12 , 6 )
                          Name { nameId = 47 })))
                 (NodeInfo
                    ("test2.c" : line 12)
                    ( "test2.c" : line 12 , 1 )
                    Name { nameId = 48 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 13)
                           ( "test2.c" : line 13 , 4 )
                           Name { nameId = 49 }))
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
                                   Name { nameId = 50 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 13)
                                 ( "test2.c" : line 13 , 3 )
                                 Name { nameId = 52 })
                          , CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 13)
                                 ( "test2.c" : line 13 , 3 )
                                 Name { nameId = 53 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 3 )
                             Name { nameId = 51 }))
                   , Just
                       (CInitExpr
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 13)
                                   ( "test2.c" : line 13 , 1 )
                                   Name { nameId = 54 }))
                             (NodeInfo
                                ("test2.c" : line 13)
                                ( "test2.c" : line 13 , 1 )
                                Name { nameId = 55 }))
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 1 )
                             Name { nameId = 56 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 13)
                    ( "test2.c" : line 13 , 1 )
                    Name { nameId = 57 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 14)
                           ( "test2.c" : line 14 , 4 )
                           Name { nameId = 58 }))
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
                                   Name { nameId = 59 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 14)
                                 ( "test2.c" : line 14 , 3 )
                                 Name { nameId = 61 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 3 )
                             Name { nameId = 60 }))
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
                                      Name { nameId = 62 }))
                                (NodeInfo
                                   ("test2.c" : line 14)
                                   ( "test2.c" : line 14 , 1 )
                                   Name { nameId = 63 }))
                             (NodeInfo
                                ("test2.c" : line 14)
                                ( "test2.c" : line 14 , 1 )
                                Name { nameId = 64 }))
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 1 )
                             Name { nameId = 65 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 14)
                    ( "test2.c" : line 14 , 1 )
                    Name { nameId = 66 }))
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
                                     Name { nameId = 67 }))
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
                                             Name { nameId = 68 })))
                                    [ CPtrDeclr
                                        []
                                        (NodeInfo
                                           ("test2.c" : line 17)
                                           ( "test2.c" : line 17 , 3 )
                                           Name { nameId = 70 })
                                    ]
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 3 )
                                       Name { nameId = 69 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CStrConst
                                          "SOURCE"
                                          (NodeInfo
                                             ("test2.c" : line 17)
                                             ( "test2.c" : line 17 , 8 )
                                             Name { nameId = 71 })))
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 8 )
                                       Name { nameId = 72 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 17)
                              ( "test2.c" : line 17 , 1 )
                              Name { nameId = 73 }))
                    , CBlockDecl
                        (CDecl
                           [ CTypeSpec
                               (CCharType
                                  (NodeInfo
                                     ("test2.c" : line 18)
                                     ( "test2.c" : line 18 , 4 )
                                     Name { nameId = 74 }))
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
                                             Name { nameId = 75 })))
                                    [ CPtrDeclr
                                        []
                                        (NodeInfo
                                           ("test2.c" : line 18)
                                           ( "test2.c" : line 18 , 4 )
                                           Name { nameId = 77 })
                                    ]
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 18)
                                       ( "test2.c" : line 18 , 4 )
                                       Name { nameId = 76 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CStrConst
                                          "HIGH ENTROPY"
                                          (NodeInfo
                                             ("test2.c" : line 18)
                                             ( "test2.c" : line 18 , 14 )
                                             Name { nameId = 78 })))
                                    (NodeInfo
                                       ("test2.c" : line 18)
                                       ( "test2.c" : line 18 , 14 )
                                       Name { nameId = 79 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 18)
                              ( "test2.c" : line 18 , 1 )
                              Name { nameId = 80 }))
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
                                             Name { nameId = 81 }))
                                       (NodeInfo
                                          ("test2.c" : line 19)
                                          ( "test2.c" : line 19 , 4 )
                                          Name { nameId = 82 }))
                                    (NodeInfo
                                       ("test2.c" : line 19)
                                       ( "test2.c" : line 19 , 4 )
                                       Name { nameId = 83 }))
                                 (CConst
                                    (CStrConst
                                       "HIGH ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 19)
                                          ( "test2.c" : line 19 , 14 )
                                          Name { nameId = 84 })))
                                 (NodeInfo
                                    ("test2.c" : line 19)
                                    ( "test2.c" : line 19 , 14 )
                                    Name { nameId = 85 })))
                           (NodeInfo
                              ("test2.c" : line 19)
                              ( "test2.c" : line 19 , 1 )
                              Name { nameId = 86 }))
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
                                             Name { nameId = 87 }))
                                       (NodeInfo
                                          ("test2.c" : line 20)
                                          ( "test2.c" : line 20 , 4 )
                                          Name { nameId = 88 }))
                                    (NodeInfo
                                       ("test2.c" : line 20)
                                       ( "test2.c" : line 20 , 4 )
                                       Name { nameId = 89 }))
                                 (CConst
                                    (CStrConst
                                       "LOW ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 20)
                                          ( "test2.c" : line 20 , 13 )
                                          Name { nameId = 90 })))
                                 (NodeInfo
                                    ("test2.c" : line 20)
                                    ( "test2.c" : line 20 , 13 )
                                    Name { nameId = 91 })))
                           (NodeInfo
                              ("test2.c" : line 20)
                              ( "test2.c" : line 20 , 1 )
                              Name { nameId = 92 }))
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
                                             Name { nameId = 93 }))
                                       (NodeInfo
                                          ("test2.c" : line 21)
                                          ( "test2.c" : line 21 , 4 )
                                          Name { nameId = 94 }))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 4 )
                                       Name { nameId = 95 }))
                                 (CBinary
                                    CAddOp
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 1 )
                                             Name { nameId = 96 })))
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 1 )
                                             Name { nameId = 97 })))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 1 )
                                       Name { nameId = 98 }))
                                 (NodeInfo
                                    ("test2.c" : line 21)
                                    ( "test2.c" : line 21 , 1 )
                                    Name { nameId = 99 })))
                           (NodeInfo
                              ("test2.c" : line 21)
                              ( "test2.c" : line 21 , 1 )
                              Name { nameId = 100 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAssignOp
                                 (CUnary
                                    CIndOp
                                    (CVar
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 22)
                                             ( "test2.c" : line 22 , 3 )
                                             Name { nameId = 101 }))
                                       (NodeInfo
                                          ("test2.c" : line 22)
                                          ( "test2.c" : line 22 , 3 )
                                          Name { nameId = 102 }))
                                    (NodeInfo
                                       ("test2.c" : line 22)
                                       ( "test2.c" : line 22 , 3 )
                                       Name { nameId = 103 }))
                                 (CConst
                                    (CStrConst
                                       "HIGH ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 22)
                                          ( "test2.c" : line 22 , 14 )
                                          Name { nameId = 104 })))
                                 (NodeInfo
                                    ("test2.c" : line 22)
                                    ( "test2.c" : line 22 , 14 )
                                    Name { nameId = 105 })))
                           (NodeInfo
                              ("test2.c" : line 22)
                              ( "test2.c" : line 22 , 1 )
                              Name { nameId = 106 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 16)
                       ( "test2.c" : line 23 , 1 )
                       Name { nameId = 107 }))
                 (NodeInfo
                    ("test2.c" : line 15)
                    ( "test2.c" : line 23 , 1 )
                    Name { nameId = 108 }))
          , CBlockStmt
              (CIf
                 (CBinary
                    CEqOp
                    (CVar
                       (Ident
                          "t"
                          116
                          (NodeInfo
                             ("test2.c" : line 24)
                             ( "test2.c" : line 24 , 1 )
                             Name { nameId = 109 }))
                       (NodeInfo
                          ("test2.c" : line 24)
                          ( "test2.c" : line 24 , 1 )
                          Name { nameId = 110 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 24)
                             ( "test2.c" : line 24 , 6 )
                             Name { nameId = 111 })))
                    (NodeInfo
                       ("test2.c" : line 24)
                       ( "test2.c" : line 24 , 6 )
                       Name { nameId = 112 }))
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
                                             ("test2.c" : line 26)
                                             ( "test2.c" : line 26 , 1 )
                                             Name { nameId = 113 }))
                                       (NodeInfo
                                          ("test2.c" : line 26)
                                          ( "test2.c" : line 26 , 1 )
                                          Name { nameId = 114 }))
                                    (NodeInfo
                                       ("test2.c" : line 26)
                                       ( "test2.c" : line 26 , 1 )
                                       Name { nameId = 115 }))
                                 (CConst
                                    (CStrConst
                                       "1234"
                                       (NodeInfo
                                          ("test2.c" : line 26)
                                          ( "test2.c" : line 26 , 6 )
                                          Name { nameId = 116 })))
                                 (NodeInfo
                                    ("test2.c" : line 26)
                                    ( "test2.c" : line 26 , 6 )
                                    Name { nameId = 117 })))
                           (NodeInfo
                              ("test2.c" : line 26)
                              ( "test2.c" : line 26 , 1 )
                              Name { nameId = 118 }))
                    , CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 27)
                                       ( "test2.c" : line 27 , 1 )
                                       Name { nameId = 119 }))))
                           (NodeInfo
                              ("test2.c" : line 27)
                              ( "test2.c" : line 27 , 1 )
                              Name { nameId = 120 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 25)
                       ( "test2.c" : line 28 , 1 )
                       Name { nameId = 121 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 29)
                                   ( "test2.c" : line 29 , 1 )
                                   Name { nameId = 122 }))
                             (NodeInfo
                                ("test2.c" : line 29)
                                ( "test2.c" : line 29 , 1 )
                                Name { nameId = 123 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 29)
                                   ( "test2.c" : line 29 , 6 )
                                   Name { nameId = 124 })))
                          (NodeInfo
                             ("test2.c" : line 29)
                             ( "test2.c" : line 29 , 6 )
                             Name { nameId = 125 }))
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
                                                   ("test2.c" : line 31)
                                                   ( "test2.c" : line 31 , 3 )
                                                   Name { nameId = 126 }))
                                             (NodeInfo
                                                ("test2.c" : line 31)
                                                ( "test2.c" : line 31 , 3 )
                                                Name { nameId = 127 }))
                                          (NodeInfo
                                             ("test2.c" : line 31)
                                             ( "test2.c" : line 31 , 3 )
                                             Name { nameId = 128 }))
                                       (CConst
                                          (CStrConst
                                             "dfgh"
                                             (NodeInfo
                                                ("test2.c" : line 31)
                                                ( "test2.c" : line 31 , 6 )
                                                Name { nameId = 129 })))
                                       (NodeInfo
                                          ("test2.c" : line 31)
                                          ( "test2.c" : line 31 , 6 )
                                          Name { nameId = 130 })))
                                 (NodeInfo
                                    ("test2.c" : line 31)
                                    ( "test2.c" : line 31 , 1 )
                                    Name { nameId = 131 }))
                          , CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 32)
                                             ( "test2.c" : line 32 , 1 )
                                             Name { nameId = 132 }))))
                                 (NodeInfo
                                    ("test2.c" : line 32)
                                    ( "test2.c" : line 32 , 1 )
                                    Name { nameId = 133 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 30)
                             ( "test2.c" : line 33 , 1 )
                             Name { nameId = 134 }))
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
                                                      ("test2.c" : line 36)
                                                      ( "test2.c" : line 36 , 3 )
                                                      Name { nameId = 135 }))
                                                (NodeInfo
                                                   ("test2.c" : line 36)
                                                   ( "test2.c" : line 36 , 3 )
                                                   Name { nameId = 136 }))
                                             (NodeInfo
                                                ("test2.c" : line 36)
                                                ( "test2.c" : line 36 , 3 )
                                                Name { nameId = 137 }))
                                          (CConst
                                             (CStrConst
                                                "xcvvcbn"
                                                (NodeInfo
                                                   ("test2.c" : line 36)
                                                   ( "test2.c" : line 36 , 9 )
                                                   Name { nameId = 138 })))
                                          (NodeInfo
                                             ("test2.c" : line 36)
                                             ( "test2.c" : line 36 , 9 )
                                             Name { nameId = 139 })))
                                    (NodeInfo
                                       ("test2.c" : line 36)
                                       ( "test2.c" : line 36 , 1 )
                                       Name { nameId = 140 }))
                             , CBlockStmt
                                 (CReturn
                                    (Just
                                       (CConst
                                          (CIntConst
                                             2
                                             (NodeInfo
                                                ("test2.c" : line 37)
                                                ( "test2.c" : line 37 , 1 )
                                                Name { nameId = 141 }))))
                                    (NodeInfo
                                       ("test2.c" : line 37)
                                       ( "test2.c" : line 37 , 1 )
                                       Name { nameId = 142 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 35)
                                ( "test2.c" : line 38 , 1 )
                                Name { nameId = 143 })))
                       (NodeInfo
                          ("test2.c" : line 29)
                          ( "test2.c" : line 38 , 1 )
                          Name { nameId = 144 })))
                 (NodeInfo
                    ("test2.c" : line 24)
                    ( "test2.c" : line 38 , 1 )
                    Name { nameId = 145 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 39)
                                 ( "test2.c" : line 39 , 3 )
                                 Name { nameId = 147 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 39)
                                         ( "test2.c" : line 39 , 1 )
                                         Name { nameId = 146 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 39)
                                   ( "test2.c" : line 39 , 1 )
                                   Name { nameId = 148 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CIntConst
                                      0
                                      (NodeInfo
                                         ("test2.c" : line 39)
                                         ( "test2.c" : line 39 , 1 )
                                         Name { nameId = 149 })))
                                (NodeInfo
                                   ("test2.c" : line 39)
                                   ( "test2.c" : line 39 , 1 )
                                   Name { nameId = 150 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 39)
                          ( "test2.c" : line 39 , 1 )
                          Name { nameId = 151 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 39)
                                ( "test2.c" : line 39 , 1 )
                                Name { nameId = 152 }))
                          (NodeInfo
                             ("test2.c" : line 39)
                             ( "test2.c" : line 39 , 1 )
                             Name { nameId = 153 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 39)
                                ( "test2.c" : line 39 , 2 )
                                Name { nameId = 154 })))
                       (NodeInfo
                          ("test2.c" : line 39)
                          ( "test2.c" : line 39 , 2 )
                          Name { nameId = 155 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 39)
                                ( "test2.c" : line 39 , 1 )
                                Name { nameId = 156 }))
                          (NodeInfo
                             ("test2.c" : line 39)
                             ( "test2.c" : line 39 , 1 )
                             Name { nameId = 157 }))
                       (NodeInfo
                          ("test2.c" : line 39)
                          ( "test2.c" : line 39 , 2 )
                          Name { nameId = 158 })))
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
                                             ("test2.c" : line 41)
                                             ( "test2.c" : line 41 , 1 )
                                             Name { nameId = 159 }))
                                       (NodeInfo
                                          ("test2.c" : line 41)
                                          ( "test2.c" : line 41 , 1 )
                                          Name { nameId = 160 }))
                                    (NodeInfo
                                       ("test2.c" : line 41)
                                       ( "test2.c" : line 41 , 1 )
                                       Name { nameId = 161 }))
                                 (CConst
                                    (CStrConst
                                       "qwer"
                                       (NodeInfo
                                          ("test2.c" : line 41)
                                          ( "test2.c" : line 41 , 6 )
                                          Name { nameId = 162 })))
                                 (NodeInfo
                                    ("test2.c" : line 41)
                                    ( "test2.c" : line 41 , 6 )
                                    Name { nameId = 163 })))
                           (NodeInfo
                              ("test2.c" : line 41)
                              ( "test2.c" : line 41 , 1 )
                              Name { nameId = 164 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 40)
                       ( "test2.c" : line 42 , 1 )
                       Name { nameId = 165 }))
                 (NodeInfo
                    ("test2.c" : line 39)
                    ( "test2.c" : line 42 , 1 )
                    Name { nameId = 166 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 43)
                           ( "test2.c" : line 43 , 3 )
                           Name { nameId = 168 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 43)
                                   ( "test2.c" : line 43 , 1 )
                                   Name { nameId = 167 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 43)
                             ( "test2.c" : line 43 , 1 )
                             Name { nameId = 169 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 43)
                                   ( "test2.c" : line 43 , 1 )
                                   Name { nameId = 170 })))
                          (NodeInfo
                             ("test2.c" : line 43)
                             ( "test2.c" : line 43 , 1 )
                             Name { nameId = 171 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 43)
                    ( "test2.c" : line 43 , 1 )
                    Name { nameId = 172 }))
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
                                   ("test2.c" : line 44)
                                   ( "test2.c" : line 44 , 1 )
                                   Name { nameId = 173 }))
                             (NodeInfo
                                ("test2.c" : line 44)
                                ( "test2.c" : line 44 , 1 )
                                Name { nameId = 174 }))
                          (NodeInfo
                             ("test2.c" : line 44)
                             ( "test2.c" : line 44 , 1 )
                             Name { nameId = 175 }))
                       (CConst
                          (CIntConst
                             4
                             (NodeInfo
                                ("test2.c" : line 44)
                                ( "test2.c" : line 44 , 1 )
                                Name { nameId = 176 })))
                       (NodeInfo
                          ("test2.c" : line 44)
                          ( "test2.c" : line 44 , 1 )
                          Name { nameId = 177 })))
                 (NodeInfo
                    ("test2.c" : line 44)
                    ( "test2.c" : line 44 , 1 )
                    Name { nameId = 178 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 45)
                             ( "test2.c" : line 45 , 1 )
                             Name { nameId = 179 }))
                       (NodeInfo
                          ("test2.c" : line 45)
                          ( "test2.c" : line 45 , 1 )
                          Name { nameId = 180 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 45)
                             ( "test2.c" : line 45 , 2 )
                             Name { nameId = 181 })))
                    (NodeInfo
                       ("test2.c" : line 45)
                       ( "test2.c" : line 45 , 2 )
                       Name { nameId = 182 }))
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
                                          ("test2.c" : line 47)
                                          ( "test2.c" : line 47 , 1 )
                                          Name { nameId = 183 }))
                                    (NodeInfo
                                       ("test2.c" : line 47)
                                       ( "test2.c" : line 47 , 1 )
                                       Name { nameId = 184 }))
                                 (NodeInfo
                                    ("test2.c" : line 47)
                                    ( "test2.c" : line 47 , 2 )
                                    Name { nameId = 185 })))
                           (NodeInfo
                              ("test2.c" : line 47)
                              ( "test2.c" : line 47 , 1 )
                              Name { nameId = 186 }))
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
                                             ("test2.c" : line 48)
                                             ( "test2.c" : line 48 , 1 )
                                             Name { nameId = 187 }))
                                       (NodeInfo
                                          ("test2.c" : line 48)
                                          ( "test2.c" : line 48 , 1 )
                                          Name { nameId = 188 }))
                                    (NodeInfo
                                       ("test2.c" : line 48)
                                       ( "test2.c" : line 48 , 1 )
                                       Name { nameId = 189 }))
                                 (CConst
                                    (CStrConst
                                       "jkl;"
                                       (NodeInfo
                                          ("test2.c" : line 48)
                                          ( "test2.c" : line 48 , 6 )
                                          Name { nameId = 190 })))
                                 (NodeInfo
                                    ("test2.c" : line 48)
                                    ( "test2.c" : line 48 , 6 )
                                    Name { nameId = 191 })))
                           (NodeInfo
                              ("test2.c" : line 48)
                              ( "test2.c" : line 48 , 1 )
                              Name { nameId = 192 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 46)
                       ( "test2.c" : line 49 , 1 )
                       Name { nameId = 193 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 45)
                    ( "test2.c" : line 49 , 1 )
                    Name { nameId = 194 }))
          ]
          (NodeInfo
             ("test2.c" : line 10)
             ( "test2.c" : line 50 , 1 )
             Name { nameId = 195 }))
       (NodeInfo
          ("test2.c" : line 9)
          ( "test2.c" : line 50 , 1 )
          Name { nameId = 196 }))
]