{
module Main where
}

%name interface
%tokentype { Token }
%error { syntaxError }

%token
        LIDENT          { LIDENT $$ }
        UIDENT          { UIDENT $$ }

%%

signature
        : {- empty -}                   { [] }
        | signatureItem signature      { $1 : $2 }

signatureItem
        : valueDescription                     { $1 }
        | primitiveDeclaration                 { $1 }
        | typeDeclarations                     { $1 }
        | sigTypeExtension                     { $1 }
        | sigExceptionDeclaration              { $1 }
        | moduleDeclaration                    { $1 }
        | moduleAlias                          { $1 }
        | recModuleDeclarations                { SigRecModule (reverse $1) }
        | moduleTypeDeclaration                { $1 }
        | openStatement                        { $1 }
        | sigIncludeStatement                  { $1 }
        | classDescriptions                    { $1 }
        | classTypeDeclarations                { $1 }

moduleAlias
        : "module" UIDENT "=" modLongident     { ModuleDeclaration {mdName = $2, mdType = ModuleType MtyAlias $4} }

recModuleDeclarations
        : recModuleDeclaration                          { [$1] }
        | recModuleDeclarations andModuleDeclaration    { $2 : $1 }

recModuleDeclaration
        : "module" "rec" UIDENT ":" moduleType          { ModuleDeclaration {mdName = $3, mdType = $5} }

andModuleDeclarations
        : "and" UIDENT ':' moduleType                   { ModuleDeclaration {mdName = $2, mdType = $4} }

ident
        : UIDENT        { $1 }
        | LIDENT        { $1 }

moduleTypeDeclarationBody
        : {- empty -}           { Nothing }
        | '=' moduleType        { Just $2 }

moduleTypeDeclaration
        : "module" "type" ident moduleTypeDeclarationBody { ModuleTypeDeclaration {mtdName = $3, mtdType = $4} }

moduleDeclaration
        : "module" UIDENT moduleDeclarationBody { ModuleDeclaration {mdName = $2, mdType = $3} }

sigTypeExtension
        : "type" optionalTypeParameters typeLongident "+=" privateFlag sigExtensionConstructors
         { TypeExtension {tyextPath = $4, tyextParams = $3, tyextConstructors = $7, tyextPrivate = $6} }

labelDeclaration
        : mutableFlag label ':' polyType   { LabelDeclaration {ldName = $2, ldMutable = $1, ldType = $4} }

labelDeclarations
        : labelDeclaration                              { [$1] }
        | labelDeclarationSemi                          { [$1] }
        | labelDeclarationSemi label_declarations       { $1 : $2 }

constructorArguments
        : coreTypeList                                  { CstrTuple (reverse $1) }
        | '{' label_declarations '}'                    { CstrRecord $2 }

generalizedConstructorArguments
        : {- empty -}                                   { (CstrTuple [], Nothing) }
        | "of" constructorArguments                     { ($2, []) }
        | ':' constructorArguments "->" simpleCoreType  { ($2, $4) }
        | ':' simpleCoreType                            { (CstrTuple [], Just $2) }

typeDeclarations
        : typeDeclaration                                            { (nonrecFlag, [ty]) where (nonrecFlag, ty) = $1 }
        | typeDeclarations andTypeDeclaration                        { (nonrecFlag, $2 : tys) where (nonrecFlag, tys) = $1 }

valIdent
        : LIDENT                                { $1 }
        | '(' operator ')'                      { $2 }

valueDescription
        : "val" valIdent ':' coreType           { ValueDescription {name = $2, type = $4, prim = []} }

primitiveDeclarationBody
        : string                                { [$1] }
        | string primitiveDeclarationBody     { $1 : $2 }

primitiveDeclaration
        : "external" valIdent ':' coreType '=' primitiveDeclarationBody { ValueDescription {name = $2, type = $4, prim = $6} }

classLongident
        : LIDENT                                                            { Ident $1 }
        | modLongident '.' LIDENT                                          { Dot $1 $3 }

coreType
        : coreType2 "as" '\'' ident                                        { TypAlias $1 $4 }
        | coreType2                                                        { $1 }

coreType2
        : simpleCoreTypeOrTuple                                         { $1 }
        | '?' LIDENT ':' coreType2 "->" coreType2                         { TypArrow (Optional $2) $4 $6 }
        | OPTLABEL coreType2 "->" coreType2                               { TypArrow (Optional $1) $2 $4 }
        | LIDENT ':' coreType2 "->" coreType2                             { TypArrow (Labelled $1) $3 $5 }
        | coreType2 "->" coreType2                                        { TypArrow NoLabel $1 $3 }

coreTypeList
        : simpleCoreType                                                  { [$1] }
        | coreTypeList '*' simpleCoreType                               { $3 : $1 }

simpleCoreTypeOrTuple
        : simpleCoreType                                                  { $1 }
        | simpleCoreType '*' coreTypeList                               { TypTuple ($1 : reverse $3) }

simpleCoreType
        : simpleCoreType2                                                 { $1 }
        | '(' core_type ')'                                                 { $2 }

coreTypeCommaList
        : coreType                                                         { [$1] }
        | coreTypeCommaList ',' coreType                                { $3 : $1 }

simpleCoreType2
        : '\'' ident                                                        { TypVar $2 }
        | '_'                                                               { TypAny }
        | typeLongident                                                    { TypConstr $1 [] }
        | simpleCoreType2 typeLongident                                  { TypConstr $2 [$1] }
        | '(' core_type_comma_list ')' typeLongident                       { TypConstr $4 $2 }
        | '<' methList '>'                                                 { TypObject f c where (f, c) = $2 }
        | '<' '>'                                                           { TypObject [] Closed }
        | '#' classLongident                                               { TypClass $2 [] }
        | simpleCoreType2 '#' classLongident                             { TypClass $3 [$1] }
        | '(' coreTypeCommaList ')' '#' classLongident                  { TypClass $5 $2 }
        | '[' tagField ']'                                                 { TypVariant $2 Closed Nothing }
        | '[' '|' rowFieldList ']'                                        { TypVariant (reverse $3) Closed Nothing }
        | '[' rowField '|' rowFieldList ']'                              { TypVariant ($2 : reverse $4) Closed Nothing }
        | "[>" optBar rowFieldList ']'                                   { TypVariant (reverse $3) Open Nothing }
        | "[>" ']'                                                          { TypVariant [] Open Nothing }
        | "[<" optBar rowFieldList ']'                                   { TypVariant (reverse $3) Closed (Just []) }
        | "[<" optBar rowFieldList '>' nameTagList ']'                 { TypVariant (reverse $3) Closed (Just (reverse $5)) }
        | '(' "module" packageType ")"                                     { TypPackage $3 }

rowFieldList
        : rowField                                                         { [$1] }
        | rowFieldList '|' rowField                                      { $3 : $1 }

rowField
        : tag_field                                                         { $1 }
        | simple_core_type                                                  { RInherit $1 }

tagField
        : nameTag "of" optAmpersand amperTypeList                       { RTag $1 $3 (reverse $4) }
        | nameTag                                                          { RTag $1 True [] }

optAmpersand
        : '&'                                                               { True }
        | {- empty -}                                                       { False }

amperTypeList
        : coreType                                                         { [$1] }
        | amperTypeList '&' coreType                                     { $3 : $1 }

{
syntaxError :: [Token] -> a
syntaxError _ = error "Syntax error"

data RecFlag
     = NonRecursive
     | Recursive

data PrivateFlag
     = Private
     | Public

data OverrideFlag
     = Override
     | Fresh

data ClosedFlag
     = Closed
     | Open

data VirtualFlag
     = Virtual
     | Concrete

data MutableFlag
     = Immutable
     | Mutable

data Variance
     = Covariant
     | Contravariant
     | Invariant

data ArgLabel
     = NoLabel
     | Labelled String
     | Optional String

data Label
     = String

data LongIdent
     = Ident String
     | Dot LongIdent String
     | Apply LongIdent LongIdent

data PackageType
     = PackageType LongIdent [(LongIdent, CoreType)]

data RowField
     = RTag Label Bool [CoreType]
     | RInherit CoreType

data CoreTypeDesc
     = TypAny
     | TypVar String
     | TypArrow ArgLabel CoreType CoreType
     | TypTuple [CoreType]
     | TypConstr LongIdent [CoreType]
     | TypObject [(String, CoreType)] ClosedFlag
     | TypClass LongIdent [CoreType]
     | TypAlias CoreType String
     | TypVariant [RowField] ClosedFlag (Maybe [Label])
     | TypPoly [String] CoreType
     | TypPackage PackageType

data CoreType
     = CoreType CoreTypeDesc

data ConstructorArguments
     = CstrTuple [CoreType]
     | CstrRecord [LabelDeclaration]

data ConstructorDeclaration
     = ConstructorDeclaration { cdName :: String
                              , cdArgs :: ConstructorArguments
                              , cdRes :: Maybe CoreType
                              }

data LabelDeclaration
     = LabelDeclaration { ldName :: String
                        , ldMutable :: MutableFlag
                        , ldType :: CoreType
                        }

data TypeKind
     = Abstract
     | Variant [ConstructorDeclaration]
     | Record [LabelDeclaration]
     | Open

data ValueDescription
     = ValueDescription { valName :: String
                        , valType :: CoreType
                        , valPrim :: [String]
                        }

data TypeDeclaration
     = TypeDeclaration { typeName :: String
                       , typeParams :: [(CoreType, Variance)]
                       , typeCstrs :: [(CoreType, CoreType)]
                       , typeKind :: TypeKind
                       , typePrivate :: PrivateFlag
                       , typeManifest :: Maybe CoreType
                       }

data TypeExtension
     = TypeExtension { tyextPath :: LongIdent
                     , tyextParams :: [(CoreType, Variance)]
                     , tyextConstructors :: [ExtensionConstructor]
                     , tyextPrivate :: PrivateFlag
                     }

data ExtensionConstructor
     = ExtensionConstructor { extName :: String
                            , extKind :: ExtensionConstructorKind
                            }

data ExtensionConstructorKind
     = ExtDecl ConstructorArguments (Maybe CoreType)
     | ExtRebind LongIdent

data ModuleDeclaration
     = ModuleDeclaration { mdName :: String
                         , mdType :: ModuleType
                         }

data ModuleTypeDeclaration
     = ModuleTypeDeclaration { mtdName :: String
                             , mtdType :: Maybe ModuleType
                             }

data OpenDescription
     = OpenDescription { openLid :: LongIdent
                       , openOverride :: OverrideFlag
                       }

data IncludeDescription
     = IncludeDescription { inclMod :: ModuleType
                          }

data ClassInfos a
     = ClassDescription { ciVirt :: VirtualFlag
                        , ciParams :: [(CoreType, Variance)]
                        , ciName :: String
                        , ciExpr :: a
                        }

data ClassDescription
     = ClassInfos ClassType

data ClassTypeDeclaration
     = ClassInfos ClassType

data ModuleType
     = ModuleType ModuleTypeDesc

data ModuleTypeDesc
     = MtyIdent LongIdent
     | MtySignature Signature
     | MtyFunctor String (Maybe ModuleType) ModuleType
     | MtyWith ModuleType [WithConstraint]
     | MtyTypeOf ModuleExpr
     | MtyAlias LongIdent

data SignatureItem
     = SigValue ValueDescription
     | SigType RecFlag [TypeDeclaration]
     | SigTypeExt TypeExtension
     | SigException ExtensionConstructor
     | SigModule ModuleDeclaration
     | SigRecModule [ModuleDeclaration]
     | SigModType ModuleTypeDeclaration
     | SigOpen OpenDescription
     | SigInclude IncludeDescription
     | SigClass ClassDescription
     | SigClassType [ClassTypeDeclaration]

data Signature
     = Signature [SignatureItem]
}
