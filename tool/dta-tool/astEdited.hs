[ CFDefExt
    (CFunDef
       [ CTypeSpec
           (CIntType
              (NodeInfo
                 ("test3.c" : line 1)
                 ( "test3.c" : line 1 , 3 )
                 Name { nameId = 1 }))
       ]
       (CDeclr
          (Just
             (Ident
                "main"
                232419565
                (NodeInfo
                   ("test3.c" : line 1)
                   ( "test3.c" : line 1 , 4 )
                   Name { nameId = 0 })))
          [ CFunDeclr
              (Right
                 ( [ CDecl
                       [ CTypeSpec
                           (CIntType
                              (NodeInfo
                                 ("test3.c" : line 1)
                                 ( "test3.c" : line 1 , 3 )
                                 Name { nameId = 4 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argc"
                                      209320289
                                      (NodeInfo
                                         ("test3.c" : line 1)
                                         ( "test3.c" : line 1 , 4 )
                                         Name { nameId = 3 })))
                                []
                                Nothing
                                []
                                (NodeInfo
                                   ("test3.c" : line 1)
                                   ( "test3.c" : line 1 , 4 )
                                   Name { nameId = 5 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test3.c" : line 1)
                          ( "test3.c" : line 1 , 4 )
                          Name { nameId = 6 })
                   , CDecl
                       [ CTypeSpec
                           (CCharType
                              (NodeInfo
                                 ("test3.c" : line 1)
                                 ( "test3.c" : line 1 , 4 )
                                 Name { nameId = 7 }))
                       ]
                       [ ( Just
                             (CDeclr
                                (Just
                                   (Ident
                                      "argv"
                                      249166177
                                      (NodeInfo
                                         ("test3.c" : line 1)
                                         ( "test3.c" : line 1 , 4 )
                                         Name { nameId = 8 })))
                                [ CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test3.c" : line 1)
                                       ( "test3.c" : line 1 , 4 )
                                       Name { nameId = 10 })
                                , CPtrDeclr
                                    []
                                    (NodeInfo
                                       ("test3.c" : line 1)
                                       ( "test3.c" : line 1 , 4 )
                                       Name { nameId = 11 })
                                ]
                                Nothing
                                []
                                (NodeInfo
                                   ("test3.c" : line 1)
                                   ( "test3.c" : line 1 , 4 )
                                   Name { nameId = 9 }))
                         , Nothing
                         , Nothing
                         )
                       ]
                       (NodeInfo
                          ("test3.c" : line 1)
                          ( "test3.c" : line 1 , 4 )
                          Name { nameId = 12 })
                   ]
                 , False
                 ))
              []
              (NodeInfo
                 ("test3.c" : line 1)
                 ( "test3.c" : line 1 , 1 )
                 Name { nameId = 13 })
          ]
          Nothing
          []
          (NodeInfo
             ("test3.c" : line 1)
             ( "test3.c" : line 1 , 4 )
             Name { nameId = 2 }))
       []
       (CCompound
          []
          [ CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test3.c" : line 2)
                           ( "test3.c" : line 2 , 4 )
                           Name { nameId = 14 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "foo"
                                1832934
                                (NodeInfo
                                   ("test3.c" : line 2)
                                   ( "test3.c" : line 2 , 3 )
                                   Name { nameId = 15 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test3.c" : line 2)
                                 ( "test3.c" : line 2 , 3 )
                                 Name { nameId = 17 })
                          , CPtrDeclr
                              []
                              (NodeInfo
                                 ("test3.c" : line 2)
                                 ( "test3.c" : line 2 , 3 )
                                 Name { nameId = 18 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test3.c" : line 2)
                             ( "test3.c" : line 2 , 3 )
                             Name { nameId = 16 }))
                   , Nothing
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test3.c" : line 2)
                    ( "test3.c" : line 2 , 1 )
                    Name { nameId = 19 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test3.c" : line 3)
                           ( "test3.c" : line 3 , 4 )
                           Name { nameId = 20 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "bar"
                                1880290
                                (NodeInfo
                                   ("test3.c" : line 3)
                                   ( "test3.c" : line 3 , 3 )
                                   Name { nameId = 21 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test3.c" : line 3)
                                 ( "test3.c" : line 3 , 3 )
                                 Name { nameId = 23 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test3.c" : line 3)
                             ( "test3.c" : line 3 , 3 )
                             Name { nameId = 22 }))
                   , Nothing
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test3.c" : line 3)
                    ( "test3.c" : line 3 , 1 )
                    Name { nameId = 24 }))
          , CBlockStmt
              (CIf
                 (CVar
                    (Ident
                       "true"
                       213743988
                       (NodeInfo
                          ("test3.c" : line 4)
                          ( "test3.c" : line 4 , 4 )
                          Name { nameId = 25 }))
                    (NodeInfo
                       ("test3.c" : line 4)
                       ( "test3.c" : line 4 , 4 )
                       Name { nameId = 26 }))
                 (CCompound
                    []
                    [ CBlockStmt
                        (CExpr
                           (Just
                              (CAssign
                                 CAssignOp
                                 (CVar
                                    (Ident
                                       "bar"
                                       1880290
                                       (NodeInfo
                                          ("test3.c" : line 5)
                                          ( "test3.c" : line 5 , 3 )
                                          Name { nameId = 27 }))
                                    (NodeInfo
                                       ("test3.c" : line 5)
                                       ( "test3.c" : line 5 , 3 )
                                       Name { nameId = 28 }))
                                 (CUnary
                                    CIndOp
                                    (CVar
                                       (Ident
                                          "foo"
                                          1832934
                                          (NodeInfo
                                             ("test3.c" : line 5)
                                             ( "test3.c" : line 5 , 3 )
                                             Name { nameId = 29 }))
                                       (NodeInfo
                                          ("test3.c" : line 5)
                                          ( "test3.c" : line 5 , 3 )
                                          Name { nameId = 30 }))
                                    (NodeInfo
                                       ("test3.c" : line 5)
                                       ( "test3.c" : line 5 , 3 )
                                       Name { nameId = 31 }))
                                 (NodeInfo
                                    ("test3.c" : line 5)
                                    ( "test3.c" : line 5 , 3 )
                                    Name { nameId = 32 })))
                           (NodeInfo
                              ("test3.c" : line 5)
                              ( "test3.c" : line 5 , 1 )
                              Name { nameId = 33 }))
                    ]
                    (NodeInfo
                       ("test3.c" : line 4)
                       ( "test3.c" : line 6 , 1 )
                       Name { nameId = 34 }))
                 (Just
                    (CIf
                       (CVar
                          (Ident
                             "false"
                             242954571
                             (NodeInfo
                                ("test3.c" : line 6)
                                ( "test3.c" : line 6 , 5 )
                                Name { nameId = 35 }))
                          (NodeInfo
                             ("test3.c" : line 6)
                             ( "test3.c" : line 6 , 5 )
                             Name { nameId = 36 }))
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
                                                   ("test3.c" : line 7)
                                                   ( "test3.c" : line 7 , 3 )
                                                   Name { nameId = 37 }))
                                             (NodeInfo
                                                ("test3.c" : line 7)
                                                ( "test3.c" : line 7 , 3 )
                                                Name { nameId = 38 }))
                                          (NodeInfo
                                             ("test3.c" : line 7)
                                             ( "test3.c" : line 7 , 3 )
                                             Name { nameId = 39 }))
                                       (CConst
                                          (CCharConst
                                             't'
                                             (NodeInfo
                                                ("test3.c" : line 7)
                                                ( "test3.c" : line 7 , 3 )
                                                Name { nameId = 40 })))
                                       (NodeInfo
                                          ("test3.c" : line 7)
                                          ( "test3.c" : line 7 , 3 )
                                          Name { nameId = 41 })))
                                 (NodeInfo
                                    ("test3.c" : line 7)
                                    ( "test3.c" : line 7 , 1 )
                                    Name { nameId = 42 }))
                          ]
                          (NodeInfo
                             ("test3.c" : line 6)
                             ( "test3.c" : line 8 , 1 )
                             Name { nameId = 43 }))
                       (Just
                          (CCompound
                             []
                             [ CBlockStmt
                                 (CExpr
                                    (Just
                                       (CBinary
                                          CAddOp
                                          (CConst
                                             (CIntConst
                                                2
                                                (NodeInfo
                                                   ("test3.c" : line 9)
                                                   ( "test3.c" : line 9 , 1 )
                                                   Name { nameId = 44 })))
                                          (CConst
                                             (CIntConst
                                                2
                                                (NodeInfo
                                                   ("test3.c" : line 9)
                                                   ( "test3.c" : line 9 , 1 )
                                                   Name { nameId = 45 })))
                                          (NodeInfo
                                             ("test3.c" : line 9)
                                             ( "test3.c" : line 9 , 1 )
                                             Name { nameId = 46 })))
                                    (NodeInfo
                                       ("test3.c" : line 9)
                                       ( "test3.c" : line 9 , 1 )
                                       Name { nameId = 47 }))
                             ]
                             (NodeInfo
                                ("test3.c" : line 8)
                                ( "test3.c" : line 10 , 1 )
                                Name { nameId = 48 })))
                       (NodeInfo
                          ("test3.c" : line 6)
                          ( "test3.c" : line 10 , 1 )
                          Name { nameId = 49 })))
                 (NodeInfo
                    ("test3.c" : line 4)
                    ( "test3.c" : line 10 , 1 )
                    Name { nameId = 50 }))
          , CBlockDecl
              (CDecl
                 [ CTypeSpec
                     (CCharType
                        (NodeInfo
                           ("test3.c" : line 11)
                           ( "test3.c" : line 11 , 4 )
                           Name { nameId = 51 }))
                 ]
                 [ ( Just
                       (CDeclr
                          (Just
                             (Ident
                                "t"
                                116
                                (NodeInfo
                                   ("test3.c" : line 11)
                                   ( "test3.c" : line 11 , 1 )
                                   Name { nameId = 52 })))
                          [ CPtrDeclr
                              []
                              (NodeInfo
                                 ("test3.c" : line 11)
                                 ( "test3.c" : line 11 , 1 )
                                 Name { nameId = 54 })
                          ]
                          Nothing
                          []
                          (NodeInfo
                             ("test3.c" : line 11)
                             ( "test3.c" : line 11 , 1 )
                             Name { nameId = 53 }))
                   , Just
                       (CInitExpr
                          (CVar
                             (Ident
                                "bar"
                                1880290
                                (NodeInfo
                                   ("test3.c" : line 11)
                                   ( "test3.c" : line 11 , 3 )
                                   Name { nameId = 55 }))
                             (NodeInfo
                                ("test3.c" : line 11)
                                ( "test3.c" : line 11 , 3 )
                                Name { nameId = 56 }))
                          (NodeInfo
                             ("test3.c" : line 11)
                             ( "test3.c" : line 11 , 3 )
                             Name { nameId = 57 }))
                   , Nothing
                   )
                 ]
                 (NodeInfo
                    ("test3.c" : line 11)
                    ( "test3.c" : line 11 , 1 )
                    Name { nameId = 58 }))
          ]
          (NodeInfo
             ("test3.c" : line 1)
             ( "test3.c" : line 12 , 1 )
             Name { nameId = 59 }))
       (NodeInfo
          ("test3.c" : line 1)
          ( "test3.c" : line 12 , 1 )
          Name { nameId = 60 }))
]