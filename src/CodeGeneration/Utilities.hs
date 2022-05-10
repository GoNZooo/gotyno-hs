module CodeGeneration.Utilities where

import RIO
import qualified RIO.Char as Char
import qualified RIO.Text as Text
import Types

upperCaseFirstCharacter :: Text -> Text
upperCaseFirstCharacter t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (Char.toUpper c) rest
    Nothing -> t

lowerCaseFirstCharacter :: Text -> Text
lowerCaseFirstCharacter t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (Char.toLower c) rest
    Nothing -> t

typeVariablesFrom :: FieldType -> Maybe [TypeVariable]
typeVariablesFrom (TypeVariableReferenceType typeVariable) = pure [typeVariable]
typeVariablesFrom (ComplexType (ArrayType _size fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (SliceType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (PointerType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (OptionalType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (RecursiveReferenceType _name) = Nothing
typeVariablesFrom (LiteralType _) = Nothing
typeVariablesFrom (BasicType _) = Nothing
typeVariablesFrom (DefinitionReferenceType definitionReference) =
  typeVariablesFromReference definitionReference

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

typeVariablesFromReference :: DefinitionReference -> Maybe [TypeVariable]
typeVariablesFromReference (DefinitionReference definition) = typeVariablesFromDefinition definition
typeVariablesFromReference (ImportedDefinitionReference _moduleName definition) =
  typeVariablesFromDefinition definition
typeVariablesFromReference (AppliedGenericReference fieldTypes _definition) =
  let typeVariables = fieldTypes & fmap typeVariablesFrom & catMaybes & join
   in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference
  ( AppliedImportedGenericReference
      _moduleName
      (AppliedTypes fieldTypes)
      _definition
    ) =
    let typeVariables = fieldTypes & fmap typeVariablesFrom & catMaybes & join
     in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference
  ( GenericDeclarationReference
      _moduleName
      _definitionName
      (AppliedTypes appliedTypes)
    ) =
    let typeVariables = appliedTypes & fmap typeVariablesFrom & catMaybes & join
     in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference (DeclarationReference _moduleName _definitionName) =
  Nothing
