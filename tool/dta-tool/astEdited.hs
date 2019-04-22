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
                "main"
                232419565
                (NodeInfo
                   ("test2.c" : line 1)
                   ( "test2.c" : line 1 , 4 )
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
                                      "argc"
                                      209320289
                                      (NodeInfo
                                         ("test2.c" : line 1)
                                         ( "test2.c" : line 1 , 4 )
                                         Name { nameId = 3 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 1)
                                   ( "test2.c" : line 1 , 4 )
                                   Name { nameId = 5 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 1)
                          ( "test2.c" : line 1 , 4 )
                          Name { nameId = 6 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test2.c" : line 1)
                                 ( "test2.c" : line 1 , 4 )
                                 Name { nameId = 7 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argv"
                                      249166177
                                      (NodeInfo
                                         ("test2.c" : line 1)
                                         ( "test2.c" : line 1 , 4 )
                                         Name { nameId = 8 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 1)
                                       ( "test2.c" : line 1 , 4 )
                                       Name { nameId = 10 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 1)
                                       ( "test2.c" : line 1 , 4 )
                                       Name { nameId = 11 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 1)
                                   ( "test2.c" : line 1 , 4 )
                                   Name { nameId = 9 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 1)
                          ( "test2.c" : line 1 , 4 )
                          Name { nameId = 12 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 1)
                 ( "test2.c" : line 1 , 1 )
                 Name { nameId = 13 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 1)
             ( "test2.c" : line 1 , 4 )
             Name { nameId = 2 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 2)
                           ( "test2.c" : line 2 , 4 )
                           Name { nameId = 14 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 2)
                                   ( "test2.c" : line 2 , 1 )
                                   Name { nameId = 15 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 2)
                                 ( "test2.c" : line 2 , 1 )
                                 Name { nameId = 17 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 2)
                             ( "test2.c" : line 2 , 1 )
                             Name { nameId = 16 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "asdf"
                                (NodeInfo
                                   ("test2.c" : line 2)
                                   ( "test2.c" : line 2 , 6 )
                                   Name { nameId = 18 })))
                          (NodeInfo
                             ("test2.c" : line 2)
                             ( "test2.c" : line 2 , 6 )
                             Name { nameId = 19 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 2)
                    ( "test2.c" : line 2 , 1 )
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
                                   ("test2.c" : line 3)
                                   ( "test2.c" : line 3 , 1 )
                                   Name { nameId = 21 }))
                             (NodeInfo
                                ("test2.c" : line 3)
                                ( "test2.c" : line 3 , 1 )
                                Name { nameId = 22 }))
                          (NodeInfo
                             ("test2.c" : line 3)
                             ( "test2.c" : line 3 , 1 )
                             Name { nameId = 23 }))
                       (CConst
                          (CStrConst
                             "zxcv"
                             (NodeInfo
                                ("test2.c" : line 3)
                                ( "test2.c" : line 3 , 6 )
                                Name { nameId = 24 })))
                       (NodeInfo
                          ("test2.c" : line 3)
                          ( "test2.c" : line 3 , 6 )
                          Name { nameId = 25 })))
                 (NodeInfo
                    ("test2.c" : line 3)
                    ( "test2.c" : line 3 , 1 )
                    Name { nameId = 26 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 4)
                           ( "test2.c" : line 4 , 4 )
                           Name { nameId = 27 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "foo"
                                1832934
                                (NodeInfo
                                   ("test2.c" : line 4)
                                   ( "test2.c" : line 4 , 3 )
                                   Name { nameId = 28 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 4)
                                 ( "test2.c" : line 4 , 3 )
                                 Name { nameId = 30 })
                          , CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 4)
                                 ( "test2.c" : line 4 , 3 )
                                 Name { nameId = 31 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 4)
                             ( "test2.c" : line 4 , 3 )
                             Name { nameId = 29 }))
                   , Just
                       (CInitExpr
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 4)
                                   ( "test2.c" : line 4 , 1 )
                                   Name { nameId = 32 }))
                             (NodeInfo
                                ("test2.c" : line 4)
                                ( "test2.c" : line 4 , 1 )
                                Name { nameId = 33 }))
                          (NodeInfo
                             ("test2.c" : line 4)
                             ( "test2.c" : line 4 , 1 )
                             Name { nameId = 34 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 4)
                    ( "test2.c" : line 4 , 1 )
                    Name { nameId = 35 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 5)
                           ( "test2.c" : line 5 , 4 )
                           Name { nameId = 36 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "bar"
                                1880290
                                (NodeInfo
                                   ("test2.c" : line 5)
                                   ( "test2.c" : line 5 , 3 )
                                   Name { nameId = 37 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 5)
                                 ( "test2.c" : line 5 , 3 )
                                 Name { nameId = 39 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 5)
                             ( "test2.c" : line 5 , 3 )
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
                                      ("test2.c" : line 5)
                                      ( "test2.c" : line 5 , 1 )
                                      Name { nameId = 40 }))
                                (NodeInfo
                                   ("test2.c" : line 5)
                                   ( "test2.c" : line 5 , 1 )
                                   Name { nameId = 41 }))
                             (NodeInfo
                                ("test2.c" : line 5)
                                ( "test2.c" : line 5 , 1 )
                                Name { nameId = 42 }))
                          (NodeInfo
                             ("test2.c" : line 5)
                             ( "test2.c" : line 5 , 1 )
                             Name { nameId = 43 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 5)
                    ( "test2.c" : line 5 , 1 )
                    Name { nameId = 44 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 6)
                           ( "test2.c" : line 6 , 3 )
                           Name { nameId = 46 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "b"
                                98
                                (NodeInfo
                                   ("test2.c" : line 6)
                                   ( "test2.c" : line 6 , 1 )
                                   Name { nameId = 45 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 6)
                             ( "test2.c" : line 6 , 1 )
                             Name { nameId = 47 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 6)
                                   ( "test2.c" : line 6 , 1 )
                                   Name { nameId = 48 })))
                          (NodeInfo
                             ("test2.c" : line 6)
                             ( "test2.c" : line 6 , 1 )
                             Name { nameId = 49 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 6)
                    ( "test2.c" : line 6 , 1 )
                    Name { nameId = 50 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 7)
                           ( "test2.c" : line 7 , 4 )
                           Name { nameId = 51 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "camelCase"
                                456381236
                                (NodeInfo
                                   ("test2.c" : line 7)
                                   ( "test2.c" : line 7 , 9 )
                                   Name { nameId = 52 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 7)
                                 ( "test2.c" : line 7 , 9 )
                                 Name { nameId = 54 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 9 )
                             Name { nameId = 53 }))
                   , Just
                       (CInitExpr
                          (CCond
                             (CVar
                                (Ident
                                   "b"
                                   98
                                   (NodeInfo
                                      ("test2.c" : line 7)
                                      ( "test2.c" : line 7 , 1 )
                                      Name { nameId = 55 }))
                                (NodeInfo
                                   ("test2.c" : line 7)
                                   ( "test2.c" : line 7 , 1 )
                                   Name { nameId = 56 }))
                             (Just
                                (CVar
                                   (Ident
                                      "bar"
                                      1880290
                                      (NodeInfo
                                         ("test2.c" : line 7)
                                         ( "test2.c" : line 7 , 3 )
                                         Name { nameId = 57 }))
                                   (NodeInfo
                                      ("test2.c" : line 7)
                                      ( "test2.c" : line 7 , 3 )
                                      Name { nameId = 58 })))
                             (CVar
                                (Ident
                                   "bar"
                                   1880290
                                   (NodeInfo
                                      ("test2.c" : line 7)
                                      ( "test2.c" : line 7 , 3 )
                                      Name { nameId = 59 }))
                                (NodeInfo
                                   ("test2.c" : line 7)
                                   ( "test2.c" : line 7 , 3 )
                                   Name { nameId = 60 }))
                             (NodeInfo
                                ("test2.c" : line 7)
                                ( "test2.c" : line 7 , 3 )
                                Name { nameId = 61 }))
                          (NodeInfo
                             ("test2.c" : line 7)
                             ( "test2.c" : line 7 , 3 )
                             Name { nameId = 62 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 7)
                    ( "test2.c" : line 7 , 1 )
                    Name { nameId = 63 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CVar
                          (Ident
                             "camelCase"
                             456381236
                             (NodeInfo
                                ("test2.c" : line 8)
                                ( "test2.c" : line 8 , 9 )
                                Name { nameId = 64 }))
                          (NodeInfo
                             ("test2.c" : line 8)
                             ( "test2.c" : line 8 , 9 )
                             Name { nameId = 65 }))
                       (CCond
                          (CVar
                             (Ident
                                "b"
                                98
                                (NodeInfo
                                   ("test2.c" : line 8)
                                   ( "test2.c" : line 8 , 1 )
                                   Name { nameId = 66 }))
                             (NodeInfo
                                ("test2.c" : line 8)
                                ( "test2.c" : line 8 , 1 )
                                Name { nameId = 67 }))
                          (Just
                             (CUnary
                                CIndOp
                                (CVar
                                   (Ident
                                      "foo"
                                      1832934
                                      (NodeInfo
                                         ("test2.c" : line 8)
                                         ( "test2.c" : line 8 , 3 )
                                         Name { nameId = 68 }))
                                   (NodeInfo
                                      ("test2.c" : line 8)
                                      ( "test2.c" : line 8 , 3 )
                                      Name { nameId = 69 }))
                                (NodeInfo
                                   ("test2.c" : line 8)
                                   ( "test2.c" : line 8 , 3 )
                                   Name { nameId = 70 })))
                          (CUnary
                             CIndOp
                             (CVar
                                (Ident
                                   "foo"
                                   1832934
                                   (NodeInfo
                                      ("test2.c" : line 8)
                                      ( "test2.c" : line 8 , 3 )
                                      Name { nameId = 71 }))
                                (NodeInfo
                                   ("test2.c" : line 8)
                                   ( "test2.c" : line 8 , 3 )
                                   Name { nameId = 72 }))
                             (NodeInfo
                                ("test2.c" : line 8)
                                ( "test2.c" : line 8 , 3 )
                                Name { nameId = 73 }))
                          (NodeInfo
                             ("test2.c" : line 8)
                             ( "test2.c" : line 8 , 3 )
                             Name { nameId = 74 }))
                       (NodeInfo
                          ("test2.c" : line 8)
                          ( "test2.c" : line 8 , 3 )
                          Name { nameId = 75 })))
                 (NodeInfo
                    ("test2.c" : line 8)
                    ( "test2.c" : line 8 , 1 )
                    Name { nameId = 76 }))
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
                               (CIntType
                                  (NodeInfo
                                     ("test2.c" : line 10)
                                     ( "test2.c" : line 10 , 3 )
                                     Name { nameId = 78 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 10)
                                             ( "test2.c" : line 10 , 3 )
                                             Name { nameId = 77 })))
                                    []
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 10)
                                       ( "test2.c" : line 10 , 3 )
                                       Name { nameId = 79 }))
                             , Nothing
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 10)
                              ( "test2.c" : line 10 , 1 )
                              Name { nameId = 80 }))
                    , CBlockDecl
                        (CDecl
                           [ CTypeSpec
                               (CIntType
                                  (NodeInfo
                                     ("test2.c" : line 11)
                                     ( "test2.c" : line 11 , 3 )
                                     Name { nameId = 82 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 11)
                                             ( "test2.c" : line 11 , 4 )
                                             Name { nameId = 81 })))
                                    []
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 11)
                                       ( "test2.c" : line 11 , 4 )
                                       Name { nameId = 83 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CIntConst
                                          34
                                          (NodeInfo
                                             ("test2.c" : line 11)
                                             ( "test2.c" : line 11 , 2 )
                                             Name { nameId = 84 })))
                                    (NodeInfo
                                       ("test2.c" : line 11)
                                       ( "test2.c" : line 11 , 2 )
                                       Name { nameId = 85 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 11)
                              ( "test2.c" : line 11 , 1 )
                              Name { nameId = 86 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAssignOp
                                 (CVar
                                    (Ident
                                       "zxcv"
                                       249101434
                                       (NodeInfo
                                          ("test2.c" : line 12)
                                          ( "test2.c" : line 12 , 4 )
                                          Name { nameId = 87 }))
                                    (NodeInfo
                                       ("test2.c" : line 12)
                                       ( "test2.c" : line 12 , 4 )
                                       Name { nameId = 88 }))
                                 (CBinary
                                    CAddOp
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 12)
                                             ( "test2.c" : line 12 , 1 )
                                             Name { nameId = 89 })))
                                    (CVar
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 12)
                                             ( "test2.c" : line 12 , 3 )
                                             Name { nameId = 90 }))
                                       (NodeInfo
                                          ("test2.c" : line 12)
                                          ( "test2.c" : line 12 , 3 )
                                          Name { nameId = 91 }))
                                    (NodeInfo
                                       ("test2.c" : line 12)
                                       ( "test2.c" : line 12 , 3 )
                                       Name { nameId = 92 }))
                                 (NodeInfo
                                    ("test2.c" : line 12)
                                    ( "test2.c" : line 12 , 3 )
                                    Name { nameId = 93 })))
                           (NodeInfo
                              ("test2.c" : line 12)
                              ( "test2.c" : line 12 , 1 )
                              Name { nameId = 95 }))
                    , CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAndAssOp
                                 (CVar
                                    (Ident
                                       "zxcv"
                                       249101434
                                       (NodeInfo
                                          ("test2.c" : line 13)
                                          ( "test2.c" : line 13 , 4 )
                                          Name { nameId = 94 }))
                                    (NodeInfo
                                       ("test2.c" : line 13)
                                       ( "test2.c" : line 13 , 4 )
                                       Name { nameId = 96 }))
                                 (CBinary
                                    CAddOp
                                    (CVar
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 13)
                                             ( "test2.c" : line 13 , 3 )
                                             Name { nameId = 97 }))
                                       (NodeInfo
                                          ("test2.c" : line 13)
                                          ( "test2.c" : line 13 , 3 )
                                          Name { nameId = 98 }))
                                    (CConst
                                       (CIntConst
                                          3
                                          (NodeInfo
                                             ("test2.c" : line 13)
                                             ( "test2.c" : line 13 , 1 )
                                             Name { nameId = 99 })))
                                    (NodeInfo
                                       ("test2.c" : line 13)
                                       ( "test2.c" : line 13 , 1 )
                                       Name { nameId = 100 }))
                                 (NodeInfo
                                    ("test2.c" : line 13)
                                    ( "test2.c" : line 13 , 1 )
                                    Name { nameId = 101 })))
                           (NodeInfo
                              ("test2.c" : line 13)
                              ( "test2.c" : line 13 , 1 )
                              Name { nameId = 102 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 9)
                       ( "test2.c" : line 14 , 1 )
                       Name { nameId = 103 }))
                 (NodeInfo
                    ("test2.c" : line 9)
                    ( "test2.c" : line 14 , 1 )
                    Name { nameId = 104 }))
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
                             Name { nameId = 105 }))
                       (NodeInfo
                          ("test2.c" : line 15)
                          ( "test2.c" : line 15 , 1 )
                          Name { nameId = 106 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 15)
                             ( "test2.c" : line 15 , 6 )
                             Name { nameId = 107 })))
                    (NodeInfo
                       ("test2.c" : line 15)
                       ( "test2.c" : line 15 , 6 )
                       Name { nameId = 108 }))
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
                                             ("test2.c" : line 16)
                                             ( "test2.c" : line 16 , 1 )
                                             Name { nameId = 109 }))
                                       (NodeInfo
                                          ("test2.c" : line 16)
                                          ( "test2.c" : line 16 , 1 )
                                          Name { nameId = 110 }))
                                    (NodeInfo
                                       ("test2.c" : line 16)
                                       ( "test2.c" : line 16 , 1 )
                                       Name { nameId = 111 }))
                                 (CConst
                                    (CStrConst
                                       "1234"
                                       (NodeInfo
                                          ("test2.c" : line 16)
                                          ( "test2.c" : line 16 , 6 )
                                          Name { nameId = 112 })))
                                 (NodeInfo
                                    ("test2.c" : line 16)
                                    ( "test2.c" : line 16 , 6 )
                                    Name { nameId = 113 })))
                           (NodeInfo
                              ("test2.c" : line 16)
                              ( "test2.c" : line 16 , 1 )
                              Name { nameId = 114 }))
                    , CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 17)
                                       ( "test2.c" : line 17 , 1 )
                                       Name { nameId = 115 }))))
                           (NodeInfo
                              ("test2.c" : line 17)
                              ( "test2.c" : line 17 , 1 )
                              Name { nameId = 116 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 15)
                       ( "test2.c" : line 18 , 1 )
                       Name { nameId = 117 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 18)
                                   ( "test2.c" : line 18 , 1 )
                                   Name { nameId = 118 }))
                             (NodeInfo
                                ("test2.c" : line 18)
                                ( "test2.c" : line 18 , 1 )
                                Name { nameId = 119 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 18)
                                   ( "test2.c" : line 18 , 6 )
                                   Name { nameId = 120 })))
                          (NodeInfo
                             ("test2.c" : line 18)
                             ( "test2.c" : line 18 , 6 )
                             Name { nameId = 121 }))
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
                                                   ("test2.c" : line 19)
                                                   ( "test2.c" : line 19 , 3 )
                                                   Name { nameId = 122 }))
                                             (NodeInfo
                                                ("test2.c" : line 19)
                                                ( "test2.c" : line 19 , 3 )
                                                Name { nameId = 123 }))
                                          (NodeInfo
                                             ("test2.c" : line 19)
                                             ( "test2.c" : line 19 , 3 )
                                             Name { nameId = 124 }))
                                       (CUnary
                                          CIndOp
                                          (CVar
                                             (Ident
                                                "foo"
                                                1832934
                                                (NodeInfo
                                                   ("test2.c" : line 19)
                                                   ( "test2.c" : line 19 , 3 )
                                                   Name { nameId = 125 }))
                                             (NodeInfo
                                                ("test2.c" : line 19)
                                                ( "test2.c" : line 19 , 3 )
                                                Name { nameId = 126 }))
                                          (NodeInfo
                                             ("test2.c" : line 19)
                                             ( "test2.c" : line 19 , 3 )
                                             Name { nameId = 127 }))
                                       (NodeInfo
                                          ("test2.c" : line 19)
                                          ( "test2.c" : line 19 , 3 )
                                          Name { nameId = 128 })))
                                 (NodeInfo
                                    ("test2.c" : line 19)
                                    ( "test2.c" : line 19 , 1 )
                                    Name { nameId = 129 }))
                          , CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 20)
                                             ( "test2.c" : line 20 , 1 )
                                             Name { nameId = 130 }))))
                                 (NodeInfo
                                    ("test2.c" : line 20)
                                    ( "test2.c" : line 20 , 1 )
                                    Name { nameId = 131 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 18)
                             ( "test2.c" : line 21 , 1 )
                             Name { nameId = 132 }))
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
                                                   "bar"
                                                   1880290
                                                   (NodeInfo
                                                      ("test2.c" : line 22)
                                                      ( "test2.c" : line 22 , 3 )
                                                      Name { nameId = 133 }))
                                                (NodeInfo
                                                   ("test2.c" : line 22)
                                                   ( "test2.c" : line 22 , 3 )
                                                   Name { nameId = 134 }))
                                             (NodeInfo
                                                ("test2.c" : line 22)
                                                ( "test2.c" : line 22 , 3 )
                                                Name { nameId = 135 }))
                                          (CConst
                                             (CIntConst
                                                34
                                                (NodeInfo
                                                   ("test2.c" : line 22)
                                                   ( "test2.c" : line 22 , 2 )
                                                   Name { nameId = 136 })))
                                          (NodeInfo
                                             ("test2.c" : line 22)
                                             ( "test2.c" : line 22 , 2 )
                                             Name { nameId = 137 })))
                                    (NodeInfo
                                       ("test2.c" : line 22)
                                       ( "test2.c" : line 22 , 1 )
                                       Name { nameId = 138 }))
                             , CBlockStmt
                                 (CExpr
                                    (Just
                                       (CAssign
                                          CAssignOp
                                          (CUnary
                                             CIndOp
                                             (CUnary
                                                CIndOp
                                                (CVar
                                                   (Ident
                                                      "foo"
                                                      1832934
                                                      (NodeInfo
                                                         ("test2.c" : line 23)
                                                         ( "test2.c" : line 23 , 3 )
                                                         Name { nameId = 139 }))
                                                   (NodeInfo
                                                      ("test2.c" : line 23)
                                                      ( "test2.c" : line 23 , 3 )
                                                      Name { nameId = 140 }))
                                                (NodeInfo
                                                   ("test2.c" : line 23)
                                                   ( "test2.c" : line 23 , 3 )
                                                   Name { nameId = 141 }))
                                             (NodeInfo
                                                ("test2.c" : line 23)
                                                ( "test2.c" : line 23 , 3 )
                                                Name { nameId = 142 }))
                                          (CConst
                                             (CStrConst
                                                "xcvvcbn"
                                                (NodeInfo
                                                   ("test2.c" : line 23)
                                                   ( "test2.c" : line 23 , 9 )
                                                   Name { nameId = 143 })))
                                          (NodeInfo
                                             ("test2.c" : line 23)
                                             ( "test2.c" : line 23 , 9 )
                                             Name { nameId = 144 })))
                                    (NodeInfo
                                       ("test2.c" : line 23)
                                       ( "test2.c" : line 23 , 1 )
                                       Name { nameId = 145 }))
                             , CBlockStmt
                                 (CReturn
                                    (Just
                                       (CConst
                                          (CIntConst
                                             2
                                             (NodeInfo
                                                ("test2.c" : line 24)
                                                ( "test2.c" : line 24 , 1 )
                                                Name { nameId = 146 }))))
                                    (NodeInfo
                                       ("test2.c" : line 24)
                                       ( "test2.c" : line 24 , 1 )
                                       Name { nameId = 147 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 21)
                                ( "test2.c" : line 25 , 1 )
                                Name { nameId = 148 })))
                       (NodeInfo
                          ("test2.c" : line 18)
                          ( "test2.c" : line 25 , 1 )
                          Name { nameId = 149 })))
                 (NodeInfo
                    ("test2.c" : line 15)
                    ( "test2.c" : line 25 , 1 )
                    Name { nameId = 150 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 26)
                                 ( "test2.c" : line 26 , 3 )
                                 Name { nameId = 152 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 26)
                                         ( "test2.c" : line 26 , 1 )
                                         Name { nameId = 151 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 26)
                                   ( "test2.c" : line 26 , 1 )
                                   Name { nameId = 153 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CIntConst
                                      0
                                      (NodeInfo
                                         ("test2.c" : line 26)
                                         ( "test2.c" : line 26 , 1 )
                                         Name { nameId = 154 })))
                                (NodeInfo
                                   ("test2.c" : line 26)
                                   ( "test2.c" : line 26 , 1 )
                                   Name { nameId = 155 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 26)
                          ( "test2.c" : line 26 , 1 )
                          Name { nameId = 156 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 26)
                                ( "test2.c" : line 26 , 1 )
                                Name { nameId = 157 }))
                          (NodeInfo
                             ("test2.c" : line 26)
                             ( "test2.c" : line 26 , 1 )
                             Name { nameId = 158 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 26)
                                ( "test2.c" : line 26 , 2 )
                                Name { nameId = 159 })))
                       (NodeInfo
                          ("test2.c" : line 26)
                          ( "test2.c" : line 26 , 2 )
                          Name { nameId = 160 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 26)
                                ( "test2.c" : line 26 , 1 )
                                Name { nameId = 161 }))
                          (NodeInfo
                             ("test2.c" : line 26)
                             ( "test2.c" : line 26 , 1 )
                             Name { nameId = 162 }))
                       (NodeInfo
                          ("test2.c" : line 26)
                          ( "test2.c" : line 26 , 2 )
                          Name { nameId = 163 })))
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
                                             ("test2.c" : line 27)
                                             ( "test2.c" : line 27 , 1 )
                                             Name { nameId = 164 }))
                                       (NodeInfo
                                          ("test2.c" : line 27)
                                          ( "test2.c" : line 27 , 1 )
                                          Name { nameId = 165 }))
                                    (NodeInfo
                                       ("test2.c" : line 27)
                                       ( "test2.c" : line 27 , 1 )
                                       Name { nameId = 166 }))
                                 (CConst
                                    (CStrConst
                                       "qwer"
                                       (NodeInfo
                                          ("test2.c" : line 27)
                                          ( "test2.c" : line 27 , 6 )
                                          Name { nameId = 167 })))
                                 (NodeInfo
                                    ("test2.c" : line 27)
                                    ( "test2.c" : line 27 , 6 )
                                    Name { nameId = 168 })))
                           (NodeInfo
                              ("test2.c" : line 27)
                              ( "test2.c" : line 27 , 1 )
                              Name { nameId = 169 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 26)
                       ( "test2.c" : line 28 , 1 )
                       Name { nameId = 170 }))
                 (NodeInfo
                    ("test2.c" : line 26)
                    ( "test2.c" : line 28 , 1 )
                    Name { nameId = 171 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 29)
                           ( "test2.c" : line 29 , 3 )
                           Name { nameId = 173 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 29)
                                   ( "test2.c" : line 29 , 1 )
                                   Name { nameId = 172 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 29)
                             ( "test2.c" : line 29 , 1 )
                             Name { nameId = 174 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 29)
                                   ( "test2.c" : line 29 , 1 )
                                   Name { nameId = 175 })))
                          (NodeInfo
                             ("test2.c" : line 29)
                             ( "test2.c" : line 29 , 1 )
                             Name { nameId = 176 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 29)
                    ( "test2.c" : line 29 , 1 )
                    Name { nameId = 177 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CVar
                          (Ident
                             "k"
                             107
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 1 )
                                Name { nameId = 178 }))
                          (NodeInfo
                             ("test2.c" : line 30)
                             ( "test2.c" : line 30 , 1 )
                             Name { nameId = 179 }))
                       (CConst
                          (CIntConst
                             4
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 1 )
                                Name { nameId = 180 })))
                       (NodeInfo
                          ("test2.c" : line 30)
                          ( "test2.c" : line 30 , 1 )
                          Name { nameId = 181 })))
                 (NodeInfo
                    ("test2.c" : line 30)
                    ( "test2.c" : line 30 , 1 )
                    Name { nameId = 182 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 31)
                             ( "test2.c" : line 31 , 1 )
                             Name { nameId = 183 }))
                       (NodeInfo
                          ("test2.c" : line 31)
                          ( "test2.c" : line 31 , 1 )
                          Name { nameId = 184 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 31)
                             ( "test2.c" : line 31 , 2 )
                             Name { nameId = 185 })))
                    (NodeInfo
                       ("test2.c" : line 31)
                       ( "test2.c" : line 31 , 2 )
                       Name { nameId = 186 }))
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
                                          ("test2.c" : line 32)
                                          ( "test2.c" : line 32 , 1 )
                                          Name { nameId = 187 }))
                                    (NodeInfo
                                       ("test2.c" : line 32)
                                       ( "test2.c" : line 32 , 1 )
                                       Name { nameId = 188 }))
                                 (NodeInfo
                                    ("test2.c" : line 32)
                                    ( "test2.c" : line 32 , 2 )
                                    Name { nameId = 189 })))
                           (NodeInfo
                              ("test2.c" : line 32)
                              ( "test2.c" : line 32 , 1 )
                              Name { nameId = 190 }))
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
                                             ("test2.c" : line 33)
                                             ( "test2.c" : line 33 , 1 )
                                             Name { nameId = 191 }))
                                       (NodeInfo
                                          ("test2.c" : line 33)
                                          ( "test2.c" : line 33 , 1 )
                                          Name { nameId = 192 }))
                                    (NodeInfo
                                       ("test2.c" : line 33)
                                       ( "test2.c" : line 33 , 1 )
                                       Name { nameId = 193 }))
                                 (CConst
                                    (CStrConst
                                       "jkl;"
                                       (NodeInfo
                                          ("test2.c" : line 33)
                                          ( "test2.c" : line 33 , 6 )
                                          Name { nameId = 194 })))
                                 (NodeInfo
                                    ("test2.c" : line 33)
                                    ( "test2.c" : line 33 , 6 )
                                    Name { nameId = 195 })))
                           (NodeInfo
                              ("test2.c" : line 33)
                              ( "test2.c" : line 33 , 1 )
                              Name { nameId = 196 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 31)
                       ( "test2.c" : line 34 , 1 )
                       Name { nameId = 198 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 31)
                    ( "test2.c" : line 34 , 1 )
                    Name { nameId = 199 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CVar
                          (Ident
                             "t"
                             116
                             (NodeInfo
                                ("test2.c" : line 35)
                                ( "test2.c" : line 35 , 1 )
                                Name { nameId = 197 }))
                          (NodeInfo
                             ("test2.c" : line 35)
                             ( "test2.c" : line 35 , 1 )
                             Name { nameId = 200 }))
                       (CVar
                          (Ident
                             "bar"
                             1880290
                             (NodeInfo
                                ("test2.c" : line 35)
                                ( "test2.c" : line 35 , 3 )
                                Name { nameId = 201 }))
                          (NodeInfo
                             ("test2.c" : line 35)
                             ( "test2.c" : line 35 , 3 )
                             Name { nameId = 202 }))
                       (NodeInfo
                          ("test2.c" : line 35)
                          ( "test2.c" : line 35 , 3 )
                          Name { nameId = 203 })))
                 (NodeInfo
                    ("test2.c" : line 35)
                    ( "test2.c" : line 35 , 1 )
                    Name { nameId = 204 }))
          ]
          (NodeInfo
             ("test2.c" : line 1)
             ( "test2.c" : line 36 , 1 )
             Name { nameId = 205 }))
       (NodeInfo
          ("test2.c" : line 1)
          ( "test2.c" : line 36 , 1 )
          Name { nameId = 206 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CCharType
              (NodeInfo
                 ("test2.c" : line 38)
                 ( "test2.c" : line 38 , 4 )
                 Name { nameId = 207 }))
       ]
       (CDeclr
          (Just
             (Ident
                "g"
                103
                (NodeInfo
                   ("test2.c" : line 38)
                   ( "test2.c" : line 38 , 1 )
                   Name { nameId = 208 })))
          [ CFunDeclr
              (Right ( [] , False ))
              []
              (NodeInfo
                 ("test2.c" : line 38)
                 ( "test2.c" : line 38 , 1 )
                 Name { nameId = 210 })
          , CPtrDeclr
              []
              (NodeInfo
                 ("test2.c" : line 38)
                 ( "test2.c" : line 38 , 1 )
                 Name { nameId = 211 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 38)
             ( "test2.c" : line 38 , 1 )
             Name { nameId = 209 }))
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
                             ("test2.c" : line 39)
                             ( "test2.c" : line 39 , 16 )
                             Name { nameId = 212 }))))
                 (NodeInfo
                    ("test2.c" : line 39)
                    ( "test2.c" : line 39 , 1 )
                    Name { nameId = 213 }))
          ]
          (NodeInfo
             ("test2.c" : line 38)
             ( "test2.c" : line 40 , 1 )
             Name { nameId = 214 }))
       (NodeInfo
          ("test2.c" : line 38)
          ( "test2.c" : line 40 , 1 )
          Name { nameId = 215 }))
, CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test2.c" : line 42)
                 ( "test2.c" : line 42 , 3 )
                 Name { nameId = 217 }))
       ]
       (CDeclr
          (Just
             (Ident
                "f"
                102
                (NodeInfo
                   ("test2.c" : line 42)
                   ( "test2.c" : line 42 , 1 )
                   Name { nameId = 216 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 42)
                                 ( "test2.c" : line 42 , 3 )
                                 Name { nameId = 220 }))
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
                                         Name { nameId = 219 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 42)
                                   ( "test2.c" : line 42 , 1 )
                                   Name { nameId = 221 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 42)
                          ( "test2.c" : line 42 , 1 )
                          Name { nameId = 222 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test2.c" : line 42)
                 ( "test2.c" : line 42 , 1 )
                 Name { nameId = 223 })
          ]
          Nothing
          []
          (NodeInfo
             ("test2.c" : line 42)
             ( "test2.c" : line 42 , 1 )
             Name { nameId = 218 }))
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
                             ("test2.c" : line 43)
                             ( "test2.c" : line 43 , 1 )
                             Name { nameId = 224 }))
                       (NodeInfo
                          ("test2.c" : line 43)
                          ( "test2.c" : line 43 , 1 )
                          Name { nameId = 225 })))
                 (NodeInfo
                    ("test2.c" : line 43)
                    ( "test2.c" : line 43 , 1 )
                    Name { nameId = 226 }))
          ]
          (NodeInfo
             ("test2.c" : line 42)
             ( "test2.c" : line 44 , 1 )
             Name { nameId = 227 }))
       (NodeInfo
          ("test2.c" : line 42)
          ( "test2.c" : line 44 , 1 )
          Name { nameId = 228 }))
]