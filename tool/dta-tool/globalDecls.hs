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
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 3 )
                             Name { nameId = 51 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CStrConst
                                "SOURCE"
                                (NodeInfo
                                   ("test2.c" : line 13)
                                   ( "test2.c" : line 13 , 8 )
                                   Name { nameId = 53 })))
                          (NodeInfo
                             ("test2.c" : line 13)
                             ( "test2.c" : line 13 , 8 )
                             Name { nameId = 54 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 13)
                    ( "test2.c" : line 13 , 1 )
                    Name { nameId = 55 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 14)
                           ( "test2.c" : line 14 , 4 )
                           Name { nameId = 56 }))
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
                                   Name { nameId = 57 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 14)
                                 ( "test2.c" : line 14 , 3 )
                                 Name { nameId = 59 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 14)
                             ( "test2.c" : line 14 , 3 )
                             Name { nameId = 58 }))
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
                                      Name { nameId = 60 }))
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
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 14)
                    ( "test2.c" : line 14 , 1 )
                    Name { nameId = 64 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 15)
                           ( "test2.c" : line 15 , 3 )
                           Name { nameId = 66 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "b"
                                98
                                (NodeInfo
                                   ("test2.c" : line 15)
                                   ( "test2.c" : line 15 , 1 )
                                   Name { nameId = 65 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 15)
                             ( "test2.c" : line 15 , 1 )
                             Name { nameId = 67 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 15)
                                   ( "test2.c" : line 15 , 1 )
                                   Name { nameId = 68 })))
                          (NodeInfo
                             ("test2.c" : line 15)
                             ( "test2.c" : line 15 , 1 )
                             Name { nameId = 69 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 15)
                    ( "test2.c" : line 15 , 1 )
                    Name { nameId = 70 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test2.c" : line 16)
                           ( "test2.c" : line 16 , 4 )
                           Name { nameId = 71 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "camelCase"
                                456381236
                                (NodeInfo
                                   ("test2.c" : line 16)
                                   ( "test2.c" : line 16 , 9 )
                                   Name { nameId = 72 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test2.c" : line 16)
                                 ( "test2.c" : line 16 , 9 )
                                 Name { nameId = 74 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 16)
                             ( "test2.c" : line 16 , 9 )
                             Name { nameId = 73 }))
                   , Just
                       (CInitExpr
                          (CCond
                             (CVar
                                (Ident
                                   "b"
                                   98
                                   (NodeInfo
                                      ("test2.c" : line 16)
                                      ( "test2.c" : line 16 , 1 )
                                      Name { nameId = 75 }))
                                (NodeInfo
                                   ("test2.c" : line 16)
                                   ( "test2.c" : line 16 , 1 )
                                   Name { nameId = 76 }))
                             (Just
                                (CVar
                                   (Ident
                                      "bar"
                                      1880290
                                      (NodeInfo
                                         ("test2.c" : line 16)
                                         ( "test2.c" : line 16 , 3 )
                                         Name { nameId = 77 }))
                                   (NodeInfo
                                      ("test2.c" : line 16)
                                      ( "test2.c" : line 16 , 3 )
                                      Name { nameId = 78 })))
                             (CVar
                                (Ident
                                   "bar"
                                   1880290
                                   (NodeInfo
                                      ("test2.c" : line 16)
                                      ( "test2.c" : line 16 , 3 )
                                      Name { nameId = 79 }))
                                (NodeInfo
                                   ("test2.c" : line 16)
                                   ( "test2.c" : line 16 , 3 )
                                   Name { nameId = 80 }))
                             (NodeInfo
                                ("test2.c" : line 16)
                                ( "test2.c" : line 16 , 3 )
                                Name { nameId = 81 }))
                          (NodeInfo
                             ("test2.c" : line 16)
                             ( "test2.c" : line 16 , 3 )
                             Name { nameId = 82 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 16)
                    ( "test2.c" : line 16 , 1 )
                    Name { nameId = 83 }))
          , CBlockStmt
              (CExpr
                 (Just
                    (CAssign
                       CAssignOp
                       (CUnary
                          CIndOp
                          (CVar
                             (Ident
                                "camelCase"
                                456381236
                                (NodeInfo
                                   ("test2.c" : line 17)
                                   ( "test2.c" : line 17 , 9 )
                                   Name { nameId = 84 }))
                             (NodeInfo
                                ("test2.c" : line 17)
                                ( "test2.c" : line 17 , 9 )
                                Name { nameId = 85 }))
                          (NodeInfo
                             ("test2.c" : line 17)
                             ( "test2.c" : line 17 , 9 )
                             Name { nameId = 86 }))
                       (CCond
                          (CVar
                             (Ident
                                "b"
                                98
                                (NodeInfo
                                   ("test2.c" : line 17)
                                   ( "test2.c" : line 17 , 1 )
                                   Name { nameId = 87 }))
                             (NodeInfo
                                ("test2.c" : line 17)
                                ( "test2.c" : line 17 , 1 )
                                Name { nameId = 88 }))
                          (Just
                             (CUnary
                                CIndOp
                                (CVar
                                   (Ident
                                      "foo"
                                      1832934
                                      (NodeInfo
                                         ("test2.c" : line 17)
                                         ( "test2.c" : line 17 , 3 )
                                         Name { nameId = 89 }))
                                   (NodeInfo
                                      ("test2.c" : line 17)
                                      ( "test2.c" : line 17 , 3 )
                                      Name { nameId = 90 }))
                                (NodeInfo
                                   ("test2.c" : line 17)
                                   ( "test2.c" : line 17 , 3 )
                                   Name { nameId = 91 })))
                          (CUnary
                             CIndOp
                             (CVar
                                (Ident
                                   "foo"
                                   1832934
                                   (NodeInfo
                                      ("test2.c" : line 17)
                                      ( "test2.c" : line 17 , 3 )
                                      Name { nameId = 92 }))
                                (NodeInfo
                                   ("test2.c" : line 17)
                                   ( "test2.c" : line 17 , 3 )
                                   Name { nameId = 93 }))
                             (NodeInfo
                                ("test2.c" : line 17)
                                ( "test2.c" : line 17 , 3 )
                                Name { nameId = 94 }))
                          (NodeInfo
                             ("test2.c" : line 17)
                             ( "test2.c" : line 17 , 3 )
                             Name { nameId = 95 }))
                       (NodeInfo
                          ("test2.c" : line 17)
                          ( "test2.c" : line 17 , 3 )
                          Name { nameId = 96 })))
                 (NodeInfo
                    ("test2.c" : line 17)
                    ( "test2.c" : line 17 , 1 )
                    Name { nameId = 97 }))
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
                                     ("test2.c" : line 20)
                                     ( "test2.c" : line 20 , 4 )
                                     Name { nameId = 98 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "baz"
                                          2011362
                                          (NodeInfo
                                             ("test2.c" : line 20)
                                             ( "test2.c" : line 20 , 3 )
                                             Name { nameId = 99 })))
                                    [ CPtrDeclr
                                        []
                                        (NodeInfo
                                           ("test2.c" : line 20)
                                           ( "test2.c" : line 20 , 3 )
                                           Name { nameId = 101 })
                                    ]
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 20)
                                       ( "test2.c" : line 20 , 3 )
                                       Name { nameId = 100 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CStrConst
                                          "SOURCE"
                                          (NodeInfo
                                             ("test2.c" : line 20)
                                             ( "test2.c" : line 20 , 8 )
                                             Name { nameId = 102 })))
                                    (NodeInfo
                                       ("test2.c" : line 20)
                                       ( "test2.c" : line 20 , 8 )
                                       Name { nameId = 103 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 20)
                              ( "test2.c" : line 20 , 1 )
                              Name { nameId = 104 }))
                    , CBlockDecl
                        (CDecl
                           [ CTypeSpec
                               (CIntType
                                  (NodeInfo
                                     ("test2.c" : line 21)
                                     ( "test2.c" : line 21 , 3 )
                                     Name { nameId = 106 }))
                           ]
                           [ ( Just
                                 (CDeclr
                                    (Just
                                       (Ident
                                          "zxcv"
                                          249101434
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 4 )
                                             Name { nameId = 105 })))
                                    []
                                    Nothing
                                    []
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 4 )
                                       Name { nameId = 107 }))
                             , Just
                                 (CInitExpr
                                    (CConst
                                       (CIntConst
                                          34
                                          (NodeInfo
                                             ("test2.c" : line 21)
                                             ( "test2.c" : line 21 , 2 )
                                             Name { nameId = 108 })))
                                    (NodeInfo
                                       ("test2.c" : line 21)
                                       ( "test2.c" : line 21 , 2 )
                                       Name { nameId = 109 }))
                             , Nothing
                             )
                           ]
                           (NodeInfo
                              ("test2.c" : line 21)
                              ( "test2.c" : line 21 , 1 )
                              Name { nameId = 110 }))
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
                                             ("test2.c" : line 22)
                                             ( "test2.c" : line 22 , 4 )
                                             Name { nameId = 111 }))
                                       (NodeInfo
                                          ("test2.c" : line 22)
                                          ( "test2.c" : line 22 , 4 )
                                          Name { nameId = 112 }))
                                    (NodeInfo
                                       ("test2.c" : line 22)
                                       ( "test2.c" : line 22 , 4 )
                                       Name { nameId = 113 }))
                                 (CConst
                                    (CStrConst
                                       "SOURCE"
                                       (NodeInfo
                                          ("test2.c" : line 22)
                                          ( "test2.c" : line 22 , 8 )
                                          Name { nameId = 114 })))
                                 (NodeInfo
                                    ("test2.c" : line 22)
                                    ( "test2.c" : line 22 , 8 )
                                    Name { nameId = 115 })))
                           (NodeInfo
                              ("test2.c" : line 22)
                              ( "test2.c" : line 22 , 1 )
                              Name { nameId = 116 }))
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
                                             ("test2.c" : line 23)
                                             ( "test2.c" : line 23 , 4 )
                                             Name { nameId = 117 }))
                                       (NodeInfo
                                          ("test2.c" : line 23)
                                          ( "test2.c" : line 23 , 4 )
                                          Name { nameId = 118 }))
                                    (NodeInfo
                                       ("test2.c" : line 23)
                                       ( "test2.c" : line 23 , 4 )
                                       Name { nameId = 119 }))
                                 (CConst
                                    (CStrConst
                                       "LOW ENTROPY"
                                       (NodeInfo
                                          ("test2.c" : line 23)
                                          ( "test2.c" : line 23 , 13 )
                                          Name { nameId = 120 })))
                                 (NodeInfo
                                    ("test2.c" : line 23)
                                    ( "test2.c" : line 23 , 13 )
                                    Name { nameId = 121 })))
                           (NodeInfo
                              ("test2.c" : line 23)
                              ( "test2.c" : line 23 , 1 )
                              Name { nameId = 122 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 19)
                       ( "test2.c" : line 24 , 1 )
                       Name { nameId = 123 }))
                 (NodeInfo
                    ("test2.c" : line 18)
                    ( "test2.c" : line 24 , 1 )
                    Name { nameId = 124 }))
          , CBlockStmt
              (CIf
                 (CBinary
                    CEqOp
                    (CVar
                       (Ident
                          "t"
                          116
                          (NodeInfo
                             ("test2.c" : line 25)
                             ( "test2.c" : line 25 , 1 )
                             Name { nameId = 125 }))
                       (NodeInfo
                          ("test2.c" : line 25)
                          ( "test2.c" : line 25 , 1 )
                          Name { nameId = 126 }))
                    (CConst
                       (CStrConst
                          "qwer"
                          (NodeInfo
                             ("test2.c" : line 25)
                             ( "test2.c" : line 25 , 6 )
                             Name { nameId = 127 })))
                    (NodeInfo
                       ("test2.c" : line 25)
                       ( "test2.c" : line 25 , 6 )
                       Name { nameId = 128 }))
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
                                             Name { nameId = 129 }))
                                       (NodeInfo
                                          ("test2.c" : line 27)
                                          ( "test2.c" : line 27 , 1 )
                                          Name { nameId = 130 }))
                                    (NodeInfo
                                       ("test2.c" : line 27)
                                       ( "test2.c" : line 27 , 1 )
                                       Name { nameId = 131 }))
                                 (CConst
                                    (CStrConst
                                       "1234"
                                       (NodeInfo
                                          ("test2.c" : line 27)
                                          ( "test2.c" : line 27 , 6 )
                                          Name { nameId = 132 })))
                                 (NodeInfo
                                    ("test2.c" : line 27)
                                    ( "test2.c" : line 27 , 6 )
                                    Name { nameId = 133 })))
                           (NodeInfo
                              ("test2.c" : line 27)
                              ( "test2.c" : line 27 , 1 )
                              Name { nameId = 134 }))
                    , CBlockStmt
                        (CReturn
                           (Just
                              (CConst
                                 (CIntConst
                                    1
                                    (NodeInfo
                                       ("test2.c" : line 28)
                                       ( "test2.c" : line 28 , 1 )
                                       Name { nameId = 135 }))))
                           (NodeInfo
                              ("test2.c" : line 28)
                              ( "test2.c" : line 28 , 1 )
                              Name { nameId = 136 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 26)
                       ( "test2.c" : line 29 , 1 )
                       Name { nameId = 137 }))
                 (Just
                    (CIf
                       (CBinary
                          CEqOp
                          (CVar
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test2.c" : line 30)
                                   ( "test2.c" : line 30 , 1 )
                                   Name { nameId = 138 }))
                             (NodeInfo
                                ("test2.c" : line 30)
                                ( "test2.c" : line 30 , 1 )
                                Name { nameId = 139 }))
                          (CConst
                             (CStrConst
                                "jklo"
                                (NodeInfo
                                   ("test2.c" : line 30)
                                   ( "test2.c" : line 30 , 6 )
                                   Name { nameId = 140 })))
                          (NodeInfo
                             ("test2.c" : line 30)
                             ( "test2.c" : line 30 , 6 )
                             Name { nameId = 141 }))
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
                                                   ("test2.c" : line 32)
                                                   ( "test2.c" : line 32 , 3 )
                                                   Name { nameId = 142 }))
                                             (NodeInfo
                                                ("test2.c" : line 32)
                                                ( "test2.c" : line 32 , 3 )
                                                Name { nameId = 143 }))
                                          (NodeInfo
                                             ("test2.c" : line 32)
                                             ( "test2.c" : line 32 , 3 )
                                             Name { nameId = 144 }))
                                       (CConst
                                          (CStrConst
                                             "SOURCE"
                                             (NodeInfo
                                                ("test2.c" : line 32)
                                                ( "test2.c" : line 32 , 8 )
                                                Name { nameId = 145 })))
                                       (NodeInfo
                                          ("test2.c" : line 32)
                                          ( "test2.c" : line 32 , 8 )
                                          Name { nameId = 146 })))
                                 (NodeInfo
                                    ("test2.c" : line 32)
                                    ( "test2.c" : line 32 , 1 )
                                    Name { nameId = 147 }))
                          , CBlockStmt
                              (CReturn
                                 (Just
                                    (CConst
                                       (CIntConst
                                          0
                                          (NodeInfo
                                             ("test2.c" : line 33)
                                             ( "test2.c" : line 33 , 1 )
                                             Name { nameId = 148 }))))
                                 (NodeInfo
                                    ("test2.c" : line 33)
                                    ( "test2.c" : line 33 , 1 )
                                    Name { nameId = 149 }))
                          ]
                          (NodeInfo
                             ("test2.c" : line 31)
                             ( "test2.c" : line 34 , 1 )
                             Name { nameId = 150 }))
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
                                                      ("test2.c" : line 37)
                                                      ( "test2.c" : line 37 , 3 )
                                                      Name { nameId = 151 }))
                                                (NodeInfo
                                                   ("test2.c" : line 37)
                                                   ( "test2.c" : line 37 , 3 )
                                                   Name { nameId = 152 }))
                                             (NodeInfo
                                                ("test2.c" : line 37)
                                                ( "test2.c" : line 37 , 3 )
                                                Name { nameId = 153 }))
                                          (CConst
                                             (CIntConst
                                                34
                                                (NodeInfo
                                                   ("test2.c" : line 37)
                                                   ( "test2.c" : line 37 , 2 )
                                                   Name { nameId = 154 })))
                                          (NodeInfo
                                             ("test2.c" : line 37)
                                             ( "test2.c" : line 37 , 2 )
                                             Name { nameId = 155 })))
                                    (NodeInfo
                                       ("test2.c" : line 37)
                                       ( "test2.c" : line 37 , 1 )
                                       Name { nameId = 156 }))
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
                                                         ("test2.c" : line 38)
                                                         ( "test2.c" : line 38 , 3 )
                                                         Name { nameId = 157 }))
                                                   (NodeInfo
                                                      ("test2.c" : line 38)
                                                      ( "test2.c" : line 38 , 3 )
                                                      Name { nameId = 158 }))
                                                (NodeInfo
                                                   ("test2.c" : line 38)
                                                   ( "test2.c" : line 38 , 3 )
                                                   Name { nameId = 159 }))
                                             (NodeInfo
                                                ("test2.c" : line 38)
                                                ( "test2.c" : line 38 , 1 )
                                                Name { nameId = 160 }))
                                          (CConst
                                             (CStrConst
                                                "xcvvcbn"
                                                (NodeInfo
                                                   ("test2.c" : line 38)
                                                   ( "test2.c" : line 38 , 9 )
                                                   Name { nameId = 161 })))
                                          (NodeInfo
                                             ("test2.c" : line 38)
                                             ( "test2.c" : line 38 , 9 )
                                             Name { nameId = 162 })))
                                    (NodeInfo
                                       ("test2.c" : line 38)
                                       ( "test2.c" : line 38 , 1 )
                                       Name { nameId = 163 }))
                             , CBlockStmt
                                 (CReturn
                                    (Just
                                       (CConst
                                          (CIntConst
                                             2
                                             (NodeInfo
                                                ("test2.c" : line 39)
                                                ( "test2.c" : line 39 , 1 )
                                                Name { nameId = 164 }))))
                                    (NodeInfo
                                       ("test2.c" : line 39)
                                       ( "test2.c" : line 39 , 1 )
                                       Name { nameId = 165 }))
                             ]
                             (NodeInfo
                                ("test2.c" : line 36)
                                ( "test2.c" : line 40 , 1 )
                                Name { nameId = 166 })))
                       (NodeInfo
                          ("test2.c" : line 30)
                          ( "test2.c" : line 40 , 1 )
                          Name { nameId = 167 })))
                 (NodeInfo
                    ("test2.c" : line 25)
                    ( "test2.c" : line 40 , 1 )
                    Name { nameId = 168 }))
          , CBlockStmt
              (CFor
                 (Right
                    (CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test2.c" : line 41)
                                 ( "test2.c" : line 41 , 3 )
                                 Name { nameId = 170 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "i"
                                      105
                                      (NodeInfo
                                         ("test2.c" : line 41)
                                         ( "test2.c" : line 41 , 1 )
                                         Name { nameId = 169 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test2.c" : line 41)
                                   ( "test2.c" : line 41 , 1 )
                                   Name { nameId = 171 }))
                         , Just
                             (CInitExpr
                                (CConst
                                   (CIntConst
                                      0
                                      (NodeInfo
                                         ("test2.c" : line 41)
                                         ( "test2.c" : line 41 , 1 )
                                         Name { nameId = 172 })))
                                (NodeInfo
                                   ("test2.c" : line 41)
                                   ( "test2.c" : line 41 , 1 )
                                   Name { nameId = 173 }))
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test2.c" : line 41)
                          ( "test2.c" : line 41 , 1 )
                          Name { nameId = 174 })))
                 (Just
                    (CBinary
                       CLeOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 41)
                                ( "test2.c" : line 41 , 1 )
                                Name { nameId = 175 }))
                          (NodeInfo
                             ("test2.c" : line 41)
                             ( "test2.c" : line 41 , 1 )
                             Name { nameId = 176 }))
                       (CConst
                          (CIntConst
                             10
                             (NodeInfo
                                ("test2.c" : line 41)
                                ( "test2.c" : line 41 , 2 )
                                Name { nameId = 177 })))
                       (NodeInfo
                          ("test2.c" : line 41)
                          ( "test2.c" : line 41 , 2 )
                          Name { nameId = 178 })))
                 (Just
                    (CUnary
                       CPostIncOp
                       (CVar
                          (Ident
                             "i"
                             105
                             (NodeInfo
                                ("test2.c" : line 41)
                                ( "test2.c" : line 41 , 1 )
                                Name { nameId = 179 }))
                          (NodeInfo
                             ("test2.c" : line 41)
                             ( "test2.c" : line 41 , 1 )
                             Name { nameId = 180 }))
                       (NodeInfo
                          ("test2.c" : line 41)
                          ( "test2.c" : line 41 , 2 )
                          Name { nameId = 181 })))
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
                                             ("test2.c" : line 43)
                                             ( "test2.c" : line 43 , 1 )
                                             Name { nameId = 182 }))
                                       (NodeInfo
                                          ("test2.c" : line 43)
                                          ( "test2.c" : line 43 , 1 )
                                          Name { nameId = 183 }))
                                    (NodeInfo
                                       ("test2.c" : line 43)
                                       ( "test2.c" : line 43 , 1 )
                                       Name { nameId = 184 }))
                                 (CConst
                                    (CStrConst
                                       "qwer"
                                       (NodeInfo
                                          ("test2.c" : line 43)
                                          ( "test2.c" : line 43 , 6 )
                                          Name { nameId = 185 })))
                                 (NodeInfo
                                    ("test2.c" : line 43)
                                    ( "test2.c" : line 43 , 6 )
                                    Name { nameId = 186 })))
                           (NodeInfo
                              ("test2.c" : line 43)
                              ( "test2.c" : line 43 , 1 )
                              Name { nameId = 187 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 42)
                       ( "test2.c" : line 44 , 1 )
                       Name { nameId = 188 }))
                 (NodeInfo
                    ("test2.c" : line 41)
                    ( "test2.c" : line 44 , 1 )
                    Name { nameId = 189 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CIntType
                        (NodeInfo
                           ("test2.c" : line 45)
                           ( "test2.c" : line 45 , 3 )
                           Name { nameId = 191 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "k"
                                107
                                (NodeInfo
                                   ("test2.c" : line 45)
                                   ( "test2.c" : line 45 , 1 )
                                   Name { nameId = 190 })))
                          []
                          Nothing
                          []
                          (NodeInfo
                             ("test2.c" : line 45)
                             ( "test2.c" : line 45 , 1 )
                             Name { nameId = 192 }))
                   , Just
                       (CInitExpr
                          (CConst
                             (CIntConst
                                0
                                (NodeInfo
                                   ("test2.c" : line 45)
                                   ( "test2.c" : line 45 , 1 )
                                   Name { nameId = 193 })))
                          (NodeInfo
                             ("test2.c" : line 45)
                             ( "test2.c" : line 45 , 1 )
                             Name { nameId = 194 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test2.c" : line 45)
                    ( "test2.c" : line 45 , 1 )
                    Name { nameId = 195 }))
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
                                ("test2.c" : line 46)
                                ( "test2.c" : line 46 , 1 )
                                Name { nameId = 196 }))
                          (NodeInfo
                             ("test2.c" : line 46)
                             ( "test2.c" : line 46 , 1 )
                             Name { nameId = 197 }))
                       (CConst
                          (CIntConst
                             4
                             (NodeInfo
                                ("test2.c" : line 46)
                                ( "test2.c" : line 46 , 1 )
                                Name { nameId = 198 })))
                       (NodeInfo
                          ("test2.c" : line 46)
                          ( "test2.c" : line 46 , 1 )
                          Name { nameId = 199 })))
                 (NodeInfo
                    ("test2.c" : line 46)
                    ( "test2.c" : line 46 , 1 )
                    Name { nameId = 200 }))
          , CBlockStmt
              (CWhile
                 (CBinary
                    CLeOp
                    (CVar
                       (Ident
                          "k"
                          107
                          (NodeInfo
                             ("test2.c" : line 47)
                             ( "test2.c" : line 47 , 1 )
                             Name { nameId = 201 }))
                       (NodeInfo
                          ("test2.c" : line 47)
                          ( "test2.c" : line 47 , 1 )
                          Name { nameId = 202 }))
                    (CConst
                       (CIntConst
                          10
                          (NodeInfo
                             ("test2.c" : line 47)
                             ( "test2.c" : line 47 , 2 )
                             Name { nameId = 203 })))
                    (NodeInfo
                       ("test2.c" : line 47)
                       ( "test2.c" : line 47 , 2 )
                       Name { nameId = 204 }))
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
                                          ("test2.c" : line 49)
                                          ( "test2.c" : line 49 , 1 )
                                          Name { nameId = 205 }))
                                    (NodeInfo
                                       ("test2.c" : line 49)
                                       ( "test2.c" : line 49 , 1 )
                                       Name { nameId = 206 }))
                                 (NodeInfo
                                    ("test2.c" : line 49)
                                    ( "test2.c" : line 49 , 2 )
                                    Name { nameId = 207 })))
                           (NodeInfo
                              ("test2.c" : line 49)
                              ( "test2.c" : line 49 , 1 )
                              Name { nameId = 208 }))
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
                                             ("test2.c" : line 50)
                                             ( "test2.c" : line 50 , 1 )
                                             Name { nameId = 209 }))
                                       (NodeInfo
                                          ("test2.c" : line 50)
                                          ( "test2.c" : line 50 , 1 )
                                          Name { nameId = 210 }))
                                    (NodeInfo
                                       ("test2.c" : line 50)
                                       ( "test2.c" : line 50 , 1 )
                                       Name { nameId = 211 }))
                                 (CConst
                                    (CStrConst
                                       "jkl;"
                                       (NodeInfo
                                          ("test2.c" : line 50)
                                          ( "test2.c" : line 50 , 6 )
                                          Name { nameId = 212 })))
                                 (NodeInfo
                                    ("test2.c" : line 50)
                                    ( "test2.c" : line 50 , 6 )
                                    Name { nameId = 213 })))
                           (NodeInfo
                              ("test2.c" : line 50)
                              ( "test2.c" : line 50 , 1 )
                              Name { nameId = 214 }))
                    ]
                    (NodeInfo
                       ("test2.c" : line 48)
                       ( "test2.c" : line 51 , 1 )
                       Name { nameId = 216 }))
                 False
                 (NodeInfo
                    ("test2.c" : line 47)
                    ( "test2.c" : line 51 , 1 )
                    Name { nameId = 217 }))
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
                                ("test2.c" : line 52)
                                ( "test2.c" : line 52 , 1 )
                                Name { nameId = 215 }))
                          (NodeInfo
                             ("test2.c" : line 52)
                             ( "test2.c" : line 52 , 1 )
                             Name { nameId = 218 }))
                       (CVar
                          (Ident
                             "bar"
                             1880290
                             (NodeInfo
                                ("test2.c" : line 52)
                                ( "test2.c" : line 52 , 3 )
                                Name { nameId = 219 }))
                          (NodeInfo
                             ("test2.c" : line 52)
                             ( "test2.c" : line 52 , 3 )
                             Name { nameId = 220 }))
                       (NodeInfo
                          ("test2.c" : line 52)
                          ( "test2.c" : line 52 , 3 )
                          Name { nameId = 221 })))
                 (NodeInfo
                    ("test2.c" : line 52)
                    ( "test2.c" : line 52 , 1 )
                    Name { nameId = 222 }))
          ]
          (NodeInfo
             ("test2.c" : line 10)
             ( "test2.c" : line 53 , 1 )
             Name { nameId = 223 }))
       (NodeInfo
          ("test2.c" : line 9)
          ( "test2.c" : line 53 , 1 )
          Name { nameId = 224 }))
]