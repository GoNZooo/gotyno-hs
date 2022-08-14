module CodeGeneration.Utilities where

import Qtility
import Types

class HasName a where
  nameOf :: a -> Text

instance HasName ModuleName where
  {-# INLINE nameOf #-}
  nameOf = (^. unwrap)

instance HasName DefinitionName where
  {-# INLINE nameOf #-}
  nameOf = (^. unwrap)

instance HasName ConstructorName where
  {-# INLINE nameOf #-}
  nameOf = (^. unwrap)

instance HasName Module where
  {-# INLINE nameOf #-}
  nameOf = moduleNameOf >>> nameOf

instance HasName Import where
  {-# INLINE nameOf #-}
  nameOf = moduleNameOf >>> nameOf

instance HasName TypeDefinition where
  {-# INLINE nameOf #-}
  nameOf = definitionNameOf >>> nameOf

instance HasName ImportedTypeDefinition where
  {-# INLINE nameOf #-}
  nameOf = definitionNameOf >>> nameOf

instance HasName DefinitionReference where
  {-# INLINE nameOf #-}
  nameOf = definitionNameOf >>> nameOf

class HasDefinitionName a where
  definitionNameOf :: a -> DefinitionName

instance HasDefinitionName DefinitionName where
  {-# INLINE definitionNameOf #-}
  definitionNameOf = id

instance HasDefinitionName TypeDefinition where
  {-# INLINE definitionNameOf #-}
  definitionNameOf t = t ^. typeDefinitionName

instance HasDefinitionName ImportedTypeDefinition where
  {-# INLINE definitionNameOf #-}
  definitionNameOf i = i ^. importedTypeDefinitionName

instance HasDefinitionName DefinitionReference where
  {-# INLINE definitionNameOf #-}
  definitionNameOf (DefinitionReference t) = definitionNameOf t
  definitionNameOf (ImportedDefinitionReference _ t) = definitionNameOf t
  definitionNameOf (AppliedGenericReference _ t) = definitionNameOf t
  definitionNameOf (AppliedImportedGenericReference _ _ t) = definitionNameOf t
  definitionNameOf (DeclarationReference _ n) = n
  definitionNameOf (GenericDeclarationReference _ n _) = n

class HasModuleName a where
  moduleNameOf :: a -> ModuleName

instance HasModuleName ModuleName where
  {-# INLINE moduleNameOf #-}
  moduleNameOf = id

instance HasModuleName Module where
  {-# INLINE moduleNameOf #-}
  moduleNameOf = (^. moduleName)

instance HasModuleName Import where
  {-# INLINE moduleNameOf #-}
  moduleNameOf = (^. unwrap . moduleName)

class MaybeHasTypeVariables a where
  typeVariablesFrom :: a -> Maybe [TypeVariable]

instance MaybeHasTypeVariables FieldType where
  {-# INLINE typeVariablesFrom #-}
  typeVariablesFrom = typeVariablesFromFieldType

instance MaybeHasTypeVariables TypeDefinition where
  {-# INLINE typeVariablesFrom #-}
  typeVariablesFrom = typeVariablesFromDefinition

instance MaybeHasTypeVariables DefinitionReference where
  {-# INLINE typeVariablesFrom #-}
  typeVariablesFrom = typeVariablesFromReference

instance MaybeHasTypeVariables UnionType where
  {-# INLINE typeVariablesFrom #-}
  typeVariablesFrom (PlainUnion _constructors) = Nothing
  typeVariablesFrom (GenericUnion ts _constructors) = Just ts

{-# INLINE typeVariablesFromFieldType #-}
typeVariablesFromFieldType :: FieldType -> Maybe [TypeVariable]
typeVariablesFromFieldType (TypeVariableReferenceType typeVariable) = pure [typeVariable]
typeVariablesFromFieldType (ComplexType (ArrayType _size fieldType)) =
  typeVariablesFromFieldType fieldType
typeVariablesFromFieldType (ComplexType (SliceType fieldType)) =
  typeVariablesFromFieldType fieldType
typeVariablesFromFieldType (ComplexType (PointerType fieldType)) =
  typeVariablesFromFieldType fieldType
typeVariablesFromFieldType (ComplexType (OptionalType fieldType)) =
  typeVariablesFromFieldType fieldType
typeVariablesFromFieldType (RecursiveReferenceType _name) = Nothing
typeVariablesFromFieldType (LiteralType _) = Nothing
typeVariablesFromFieldType (BasicType _) = Nothing
typeVariablesFromFieldType (DefinitionReferenceType definitionReference) =
  typeVariablesFromReference definitionReference

{-# INLINE typeVariablesFromDefinition #-}
typeVariablesFromDefinition :: TypeDefinition -> Maybe [TypeVariable]
typeVariablesFromDefinition (TypeDefinition _name (Struct (PlainStruct _))) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Union _tagType (PlainUnion _))) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (UntaggedUnion _)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Enumeration _)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (EmbeddedUnion _tagType _constructors)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Struct (GenericStruct typeVariables _))) =
  pure typeVariables
typeVariablesFromDefinition (TypeDefinition _name (Union _tagType (GenericUnion typeVariables _))) =
  pure typeVariables
typeVariablesFromDefinition (TypeDefinition _name (DeclaredType _moduleName typeVariables)) =
  pure typeVariables

{-# INLINE typeVariablesFromReference #-}
typeVariablesFromReference :: DefinitionReference -> Maybe [TypeVariable]
typeVariablesFromReference (DefinitionReference definition) = typeVariablesFrom definition
typeVariablesFromReference (ImportedDefinitionReference _moduleName definition) =
  typeVariablesFrom definition
typeVariablesFromReference (AppliedGenericReference fieldTypes _definition) =
  let typeVariables = fieldTypes & mapMaybe typeVariablesFrom & join
   in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference
  ( AppliedImportedGenericReference
      _moduleName
      (AppliedTypes fieldTypes)
      _definition
    ) =
    let typeVariables = fieldTypes & mapMaybe typeVariablesFrom & join
     in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference
  ( GenericDeclarationReference
      _moduleName
      _definitionName
      (AppliedTypes appliedTypes)
    ) =
    let typeVariables = appliedTypes & mapMaybe typeVariablesFrom & join
     in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference (DeclarationReference _moduleName _definitionName) =
  Nothing

{-# INLINE structFieldsFromReference #-}
structFieldsFromReference :: DefinitionReference -> [StructField]
structFieldsFromReference
  (DefinitionReference (TypeDefinition _name (Struct (PlainStruct fields)))) = fields
structFieldsFromReference _other = error "struct fields from anything other than plain struct"
