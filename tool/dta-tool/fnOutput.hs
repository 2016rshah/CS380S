CFDefExt (
    CFunDef 
        [CTypeSpec (CIntType (NodeInfo ("test2.c": line 42) (("test2.c": line 42),3) (Name {nameId = 1})))] 
        (CDeclr (Just (Ident "f" 102 (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 0})))) 
                [CFunDeclr 
                    (Right ([CDecl 
                                [CTypeSpec (CIntType (NodeInfo ("test2.c": line 42) (("test2.c": line 42),3) (Name {nameId = 4})))] 
                                [(Just 
                                    (CDeclr 
                                        (Just (Ident "k" 107 (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 3})))) 
                                        [] 
                                        Nothing 
                                        [] 
                                        (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 5}))),
                                  Nothing,
                                  Nothing)
                                ] 
                                (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 6}))
                               ],
                               False
                            )
                    ) 
                    [] 
                    (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 7}))
                ] 
                Nothing 
                [] 
                (NodeInfo ("test2.c": line 42) (("test2.c": line 42),1) (Name {nameId = 2}))
        ) 
        [] 
        (CCompound 
            [] 
            [CBlockStmt 
                (CReturn 
                    (Just (CVar 
                        (Ident "k" 107 (NodeInfo ("test2.c": line 43) (("test2.c": line 43),1) (Name {nameId = 8}))) 
                        (NodeInfo ("test2.c": line 43) (("test2.c": line 43),1) (Name {nameId = 9})))) (NodeInfo ("test2.c": line 43) (("test2.c": line 43),1) (Name {nameId = 10})))
            ]
            (NodeInfo ("test2.c": line 42) (("test2.c": line 44),1) (Name {nameId = 11}))
        ) 
    NodeInfo ("test2.c": line 42) (("test2.c": line 44),1) (Name {nameId = 12})
)