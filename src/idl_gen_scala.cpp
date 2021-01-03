/*
 * Copyright 2014 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// independent from idl_parser, since this code is not needed for most clients

#include <string>

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

namespace flatbuffers {
namespace scala {
static TypedFloatConstantGenerator ScalaFloatGen("Double.", "Float.", "NaN",
                                                 "PositiveInfinity",
                                                 "NegativeInfinity");

static const CommentConfig comment_config = { "/**", " *", " */" };
static const std::string ident_pad = "  ";

class ScalaGenerator : public BaseGenerator {
 public:
  ScalaGenerator(const Parser &parser, const std::string &path,
                 const std::string &file_name)
      : BaseGenerator(parser, path, file_name, "", ".", "scala"),
        cur_name_space_(nullptr) {}

  bool generate() {
    std::string one_file_code;
    cur_name_space_ = parser_.current_namespace_;

    for (auto it = parser_.enums_.vec.begin(); it != parser_.enums_.vec.end();
         ++it) {
      CodeWriter enumWriter(ident_pad);
      auto &enum_def = **it;
      if (!parser_.opts.one_file) cur_name_space_ = enum_def.defined_namespace;
      if (enum_def.is_union) {
        GenUnion(enum_def, enumWriter);
      } else {
        GenEnum(enum_def, enumWriter);
      }
      if (parser_.opts.one_file) {
        one_file_code += enumWriter.ToString();
      } else {
        if (!SaveType(enum_def.name, *enum_def.defined_namespace,
                      enumWriter.ToString(), false))
          return false;
      }
    }

    for (auto it = parser_.structs_.vec.begin();
         it != parser_.structs_.vec.end(); ++it) {
      CodeWriter structWriter(ident_pad);
      auto &struct_def = **it;
      if (!parser_.opts.one_file)
        cur_name_space_ = struct_def.defined_namespace;
      GenStruct(struct_def, structWriter);
      if (parser_.opts.one_file) {
        one_file_code += structWriter.ToString();
      } else {
        if (!SaveType(struct_def.name, *struct_def.defined_namespace,
                      structWriter.ToString(), true))
          return false;
      }
    }

    return true;
  }

  std::string TypeBasic(const BaseType &type) const {
    // clang-format off
      static const char * const scala_typename[] = {
        #define FLATBUFFERS_TD(ENUM, IDLTYPE, \
                CTYPE, JTYPE, GTYPE, NTYPE, PTYPE, RTYPE, KTYPE, STYPE, SCTYPE) \
          #SCTYPE,
          FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
        #undef FLATBUFFERS_TD
      };
    // clang-format on
    return scala_typename[type];
  }

  std::string TypePointer(const Type &type) const {
    switch (type.base_type) {
      case BASE_TYPE_STRING: return "String";
      case BASE_TYPE_VECTOR: return "Seq[" + Type(type.VectorType()) + "]";
      case BASE_TYPE_STRUCT: return WrapInNameSpace(*type.struct_def);
      default: return "Table";
    }
  }
  std::string Type(const Type &type) const {
    if (IsEnum(type) || IsUnion(type)) {
      return WrapInNameSpace(*type.enum_def);
    } else if (IsScalar(type.base_type)) {
      return TypeBasic(type.base_type);
    } else {
      return TypePointer(type);
    }
  }

  std::string LiteralSuffix(const BaseType type) const {
    switch (type) {
      case BASE_TYPE_ULONG:
      case BASE_TYPE_LONG: return "L";
      default: return "";
    }
  }

  std::string DefaultValue(const FieldDef &field) const {
    auto &value = field.value;
    auto &type = field.value.type;
    auto base_type = field.value.type.base_type;

    if (base_type == BASE_TYPE_VECTOR) { return "Nil"; }
    if (field.IsScalarOptional()) { return "None"; }
    if (IsFloat(base_type)) {
      auto val = ScalaFloatGen.GenFloatConstant(field);
      if (base_type == BASE_TYPE_DOUBLE && val.back() == 'f') {
        val.pop_back();
      }
      return val;
    }

    if (base_type == BASE_TYPE_BOOL) {
      return value.constant == "0" ? "false" : "true";
    }

    if (IsEnum(field.value.type)) {
      return WrapInNameSpace(*type.enum_def) + ".fromValue(" + value.constant +
             ")";
    }

    std::string suffix =
        base_type == BASE_TYPE_LONG ? LiteralSuffix(base_type) : "";
    return value.constant + suffix;
  }

  std::string ElemSize(const flatbuffers::Type &type) const {
    switch (type.base_type) {
      case BASE_TYPE_VECTOR:
        switch (type.element) {
          case BASE_TYPE_STRUCT:
            if (type.struct_def->fixed) {
              return NumToString(type.struct_def->bytesize);
            } else {
              return "sizeOfInt";
            }
          default: return NumToString(SizeOf(type.element));
        }
      default: return NumToString(SizeOf(type.element));
    }
  }

  void GenCommentWriter(const std::vector<std::string> &dc, CodeWriter &writer,
                        const char *prefix = "") const {
    std::string comment;
    GenComment(dc, &comment, &comment_config, prefix);
    if (!comment.empty()) { writer += comment; }
  }

  void GenUnion(EnumDef &enum_def, CodeWriter &writer) const {
    if (enum_def.generated) return;

    GenCommentWriter(enum_def.doc_comment, writer);
    writer.SetValue("UNION_TYPE_NAME", enum_def.name);
    writer.SetValue("UNION_SCALAR_TYPE",
                    TypeBasic(enum_def.underlying_type.base_type));
    writer += "sealed trait {{UNION_TYPE_NAME}} {";
    writer.IncrementIdentLevel();
    writer += "val value: {{UNION_SCALAR_TYPE}}";
    writer += "val name: String";
    writer.DecrementIdentLevel();
    writer += "}";
    writer += "";
    writer += "object {{UNION_TYPE_NAME}} {";
    writer.IncrementIdentLevel();
    auto vals = enum_def.Vals();
    for (auto it = vals.begin(); it != vals.end(); ++it) {
      auto &ev = **it;
      auto val = enum_def.ToString(ev);
      auto suffix = LiteralSuffix(enum_def.underlying_type.base_type);
      writer.SetValue("UNION_VALUE_NAME", ev.name);
      writer.SetValue("UNION_VALUE", val + suffix);
      writer.SetValue("UNION_DATA_TYPE", Type(ev.union_type));
      if (ev.IsZero()) {
        writer +=
            "case object {{UNION_VALUE_NAME}} extends {{UNION_TYPE_NAME}} {";
      } else {
        writer +=
            "case class {{UNION_VALUE_NAME}}(data: {{UNION_DATA_TYPE}}) "
            "extends {{UNION_TYPE_NAME}} {";
      }
      writer.IncrementIdentLevel();
      writer += "val value: {{UNION_SCALAR_TYPE}} = {{UNION_VALUE}}";
      writer += "val name: String = \"{{UNION_VALUE_NAME}}\"";
      writer.DecrementIdentLevel();
      writer += "}";
    }
    writer +=
        "implicit val getter: Getter[{{UNION_TYPE_NAME}}] = (o, bb) => "
        "__get[{{UNION_SCALAR_TYPE}}](o, bb) match {";
    writer.IncrementIdentLevel();
    for (auto it = vals.begin(); it != vals.end(); ++it) {
      auto &ev = **it;
      auto val = enum_def.ToString(ev);
      auto suffix = LiteralSuffix(enum_def.underlying_type.base_type);
      writer.SetValue("UNION_VALUE_NAME", ev.name);
      writer.SetValue("UNION_VALUE", val + suffix);
      writer.SetValue("UNION_DATA_TYPE", Type(ev.union_type));
      if (ev.IsZero()) {
        writer += "case {{UNION_VALUE}} => {{UNION_VALUE_NAME}}";
      } else {
        writer +=
            "case {{UNION_VALUE}} => "
            "{{UNION_VALUE_NAME}}({{UNION_DATA_TYPE}}(__indirect(o, bb), bb))";
      }
    }
    writer.DecrementIdentLevel();
    writer += "}";
    writer.DecrementIdentLevel();
    writer += "}";
  }

  void GenEnum(EnumDef &enum_def, CodeWriter &writer) const {
    if (enum_def.generated) return;

    GenCommentWriter(enum_def.doc_comment, writer);
    writer.SetValue("ENUM_TYPE_NAME", enum_def.name);
    writer.SetValue("ENUM_SCALAR_TYPE",
                    TypeBasic(enum_def.underlying_type.base_type));
    writer += "sealed trait {{ENUM_TYPE_NAME}} {";
    writer.IncrementIdentLevel();
    writer += "val value: {{ENUM_SCALAR_TYPE}}";
    writer += "val name: String";
    writer.DecrementIdentLevel();
    writer += "}";
    writer += "";
    writer += "object {{ENUM_TYPE_NAME}} {";
    writer.IncrementIdentLevel();
    auto vals = enum_def.Vals();
    for (auto it = vals.begin(); it != vals.end(); ++it) {
      auto &ev = **it;
      auto val = enum_def.ToString(ev);
      auto suffix = LiteralSuffix(enum_def.underlying_type.base_type);
      writer.SetValue("ENUM_VALUE_NAME", ev.name);
      writer.SetValue("ENUM_VALUE", val + suffix);
      writer += "case object {{ENUM_VALUE_NAME}} extends {{ENUM_TYPE_NAME}} {";
      writer.IncrementIdentLevel();
      writer += "val value: {{ENUM_SCALAR_TYPE}} = {{ENUM_VALUE}}";
      writer += "val name: String = \"{{ENUM_VALUE_NAME}}\"";
      writer.DecrementIdentLevel();
      writer += "}";
    }
    GenFunction(writer, "fromValue", "(value: {{ENUM_SCALAR_TYPE}})",
                enum_def.name, [&]() {
                  writer += "value match {";
                  writer.IncrementIdentLevel();
                  for (auto it = vals.begin(); it != vals.end(); ++it) {
                    auto &ev = **it;
                    auto val = enum_def.ToString(ev);
                    auto suffix =
                        LiteralSuffix(enum_def.underlying_type.base_type);
                    writer.SetValue("ENUM_VALUE_NAME", ev.name);
                    writer.SetValue("ENUM_VALUE", val + suffix);
                    writer += "case {{ENUM_VALUE}} => {{ENUM_VALUE_NAME}}";
                  }
                  writer.DecrementIdentLevel();
                  writer += "}";
                });
    writer +=
        "implicit val getter: Getter[{{ENUM_TYPE_NAME}}] = (o, bb) => "
        "fromValue(__get[{{ENUM_SCALAR_TYPE}}](o, bb))";
    writer.DecrementIdentLevel();
    writer += "}";
  }

  void GenFunction(CodeWriter &writer, const std::string &name,
                   const std::string &params, const std::string &return_type,
                   const std::function<void()> &body) const {
    writer += "def " + name + params + ": " + return_type + " =";
    writer.IncrementIdentLevel();
    body();
    writer.DecrementIdentLevel();
  }

  std::string BBGet(const flatbuffers::Type &type,
                    const std::string &offset) const {
    if (IsVector(type)) {
      return "__vector[" + Type(type.VectorType()) + "](" + offset + ", bb, " +
             ElemSize(type) + ")";
    }
    return "__get[" + Type(type) + "](" + offset + ", bb)";
  }

  std::string FieldType(const FieldDef &field) const {
    std::string ret = Type(field.value.type);
    return field.optional ? "Option[" + ret + "]" : ret;
  }

  void GenStructGetters(StructDef &struct_def, CodeWriter &writer) const {
    auto fields_vec = struct_def.fields.vec;
    // FieldDef *key_field = nullptr;
    for (auto it = fields_vec.begin(); it != fields_vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      if (IsUnion(field.value.type) &&
          field.value.type.base_type != BASE_TYPE_UNION)
        continue;
      // if (field.key) key_field = &field;

      GenCommentWriter(field.doc_comment, writer);
      writer.SetValue("GETTER_NAME", MakeCamel(field.name, false));
      writer.SetValue("GETTER_TYPE", FieldType(field));
      writer.SetValue("GETTER_OFFSET", NumToString(field.value.offset));
      writer.SetValue("GETTER_DEFAULT", DefaultValue(field));

      std::string bb_get;
      if (struct_def.fixed) {
        bb_get = BBGet(field.value.type, "bbPos + {{GETTER_OFFSET}}");
      } else {
        bb_get = "__offset({{GETTER_OFFSET}}).map(o => " +
                 BBGet(field.value.type, "bbPos + o") + ")";
        if (!field.optional) { bb_get += ".getOrElse({{GETTER_DEFAULT}})"; }
      }
      GenFunction(writer, "{{GETTER_NAME}}", "", "{{GETTER_TYPE}}",
                  [&]() { writer += bb_get; });
    }
  }

  void GenStruct(StructDef &struct_def, CodeWriter &writer) const {
    if (struct_def.generated) return;

    std::string comment;
    GenComment(struct_def.doc_comment, &comment, &comment_config);
    writer += comment;
    writer.SetValue("STRUCT_NAME", struct_def.name);
    writer.SetValue("STRUCT_SUPER", struct_def.fixed ? "Struct" : "Table");
    writer +=
        "class {{STRUCT_NAME}}(private bbPos: Int, private bb: ByteBuffer) "
        "extends {{STRUCT_SUPER}} {";
    writer.IncrementIdentLevel();
    GenStructGetters(struct_def, writer);
    writer.DecrementIdentLevel();
    writer += "}";
    writer += "";
    writer += "object {{STRUCT_NAME}} {";
    writer.IncrementIdentLevel();
    if (struct_def.fixed) {
      writer +=
          "implicit val getter: Getter[{{STRUCT_NAME}}] = (o, bb) => "
          "{{STRUCT_NAME}}(o, bb)";
    } else {
      writer +=
          "implicit val getter: Getter[{{STRUCT_NAME}}] = (o, bb) => "
          "{{STRUCT_NAME}}(__indirect(o, bb), bb)";
    }
    writer.DecrementIdentLevel();
    writer += "}";
  }

  // Save out the generated code for a single class while adding
  // declaration boilerplate.
  bool SaveType(const std::string &defname, const Namespace &ns,
                const std::string &classcode, bool needs_includes) const {
    if (!classcode.length()) return true;

    std::string code =
        "// " + std::string(FlatBuffersGeneratedWarning()) + "\n\n";

    std::string namespace_name = FullNamespace(".", ns);
    if (!namespace_name.empty()) {
      code += "package " + namespace_name;
      code += "\n\n";
      code += "import com.google.flatbuffers._\n";
    }
    if (needs_includes) {}
    code += classcode;
    auto filename = NamespaceDir(ns) + defname + ".scala";
    return SaveFile(filename.c_str(), code, false);
  }

  // This tracks the current namespace used to determine if a type need to
  // be prefixed by its namespace
  const Namespace *cur_name_space_;
};  // namespace scala
}  // namespace scala

bool GenerateScala(const Parser &parser, const std::string &path,
                   const std::string &file_name) {
  scala::ScalaGenerator generator(parser, path, file_name);
  return generator.generate();
}
}  // namespace flatbuffers