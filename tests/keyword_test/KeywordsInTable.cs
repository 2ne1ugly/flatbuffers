// <auto-generated>
//  automatically generated by the FlatBuffers compiler, do not modify
// </auto-generated>

using global::System;
using global::System.Collections.Generic;
using global::FlatBuffers;

public struct KeywordsInTable : IFlatbufferObject
{
  private Table __p;
  public ByteBuffer ByteBuffer { get { return __p.bb; } }
  public static void ValidateVersion() { FlatBufferConstants.FLATBUFFERS_2_0_0(); }
  public static KeywordsInTable GetRootAsKeywordsInTable(ByteBuffer _bb) { return GetRootAsKeywordsInTable(_bb, new KeywordsInTable()); }
  public static KeywordsInTable GetRootAsKeywordsInTable(ByteBuffer _bb, KeywordsInTable obj) { return (obj.__assign(_bb.GetInt(_bb.Position) + _bb.Position, _bb)); }
  public void __init(int _i, ByteBuffer _bb) { __p = new Table(_i, _bb); }
  public KeywordsInTable __assign(int _i, ByteBuffer _bb) { __init(_i, _bb); return this; }

  public ABC Is { get { int o = __p.__offset(4); return o != 0 ? (ABC)__p.bb.GetInt(o + __p.bb_pos) : ABC.void_; } }
  public bool MutateIs(ABC is_) { int o = __p.__offset(4); if (o != 0) { __p.bb.PutInt(o + __p.bb_pos, (int)is_); return true; } else { return false; } }
  public public_ Private { get { int o = __p.__offset(6); return o != 0 ? (public_)__p.bb.GetInt(o + __p.bb_pos) : public_.NONE; } }
  public bool MutatePrivate(public_ private_) { int o = __p.__offset(6); if (o != 0) { __p.bb.PutInt(o + __p.bb_pos, (int)private_); return true; } else { return false; } }
  public int Type { get { int o = __p.__offset(8); return o != 0 ? __p.bb.GetInt(o + __p.bb_pos) : (int)0; } }
  public bool MutateType(int type) { int o = __p.__offset(8); if (o != 0) { __p.bb.PutInt(o + __p.bb_pos, type); return true; } else { return false; } }

  public static Offset<KeywordsInTable> CreateKeywordsInTable(FlatBufferBuilder builder,
      ABC is_ = ABC.void_,
      public_ private_ = public_.NONE,
      int type = 0) {
    builder.StartTable(3);
    KeywordsInTable.AddType(builder, type);
    KeywordsInTable.AddPrivate(builder, private_);
    KeywordsInTable.AddIs(builder, is_);
    return KeywordsInTable.EndKeywordsInTable(builder);
  }

  public static void StartKeywordsInTable(FlatBufferBuilder builder) { builder.StartTable(3); }
  public static void AddIs(FlatBufferBuilder builder, ABC is_) { builder.AddInt(0, (int)is_, 0); }
  public static void AddPrivate(FlatBufferBuilder builder, public_ private_) { builder.AddInt(1, (int)private_, 0); }
  public static void AddType(FlatBufferBuilder builder, int type) { builder.AddInt(2, type, 0); }
  public static Offset<KeywordsInTable> EndKeywordsInTable(FlatBufferBuilder builder) {
    int o = builder.EndTable();
    return new Offset<KeywordsInTable>(o);
  }
  public KeywordsInTableT UnPack() {
    var _o = new KeywordsInTableT();
    this.UnPackTo(_o);
    return _o;
  }
  public void UnPackTo(KeywordsInTableT _o) {
    _o.Is = this.Is;
    _o.Private = this.Private;
    _o.Type = this.Type;
  }
  public static Offset<KeywordsInTable> Pack(FlatBufferBuilder builder, KeywordsInTableT _o) {
    if (_o == null) return default(Offset<KeywordsInTable>);
    return CreateKeywordsInTable(
      builder,
      _o.Is,
      _o.Private,
      _o.Type);
  }
}

public class KeywordsInTableT
{
  [Newtonsoft.Json.JsonProperty("is")]
  public ABC Is { get; set; }
  [Newtonsoft.Json.JsonProperty("private")]
  public public_ Private { get; set; }
  [Newtonsoft.Json.JsonProperty("type")]
  public int Type { get; set; }

  public KeywordsInTableT() {
    this.Is = ABC.void_;
    this.Private = public_.NONE;
    this.Type = 0;
  }
}

