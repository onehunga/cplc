const Self = @This();
const std = @import("std");

tag: Tag,
data: Data = .empty,

pub fn equals(self: Self, other: Self) bool {
    if (self.tag != other.tag) {
        return false;
    }

    switch (self.tag) {
        Tag.unknown => return true,
        Tag.void => return true,
        Tag.bool => return true,
        Tag.int => return self.data.int.bits == other.data.int.bits and self.data.int.signed == other.data.int.signed,
        Tag.float => return self.data.float.bits == other.data.float.bits,
        Tag.@"struct" => {
            if (self.data.@"struct".fields.len != other.data.@"struct".fields.len) {
                return false;
            }

            for (self.data.@"struct".fields, 0..) |field, idx| {
                if (!std.mem.eql(u8, field.name, other.data.@"struct".fields[idx].name)) {
                    return false;
                }

                if (field.ty.id != other.data.@"struct".fields[idx].ty.id) {
                    return false;
                }
            }

            return true;
        },
        Tag.tuple => {
            if (self.data.tuple.fields.len != other.data.tuple.fields.len) {
                return false;
            }

            for (self.data.tuple.fields, 0..) |field, idx| {
                if (field.id != other.data.tuple.fields[idx].id) {
                    return false;
                }
            }

            return true;
        },
        Tag.slice => {
            return self.data.slice.element.id == other.data.slice.element.id;
        },
    }

    return true;
}

pub const Tag = enum {
    unknown,
    void,
    bool,
    int,
    float,
    @"struct",
    tuple,
    slice,
};

pub const Data = union {
    int: IntData,
    float: FloatData,
    @"struct": StructData,
    tuple: TupleData,
    slice: SliceData,

    pub const empty: Data = undefined;
};

pub const IntData = struct {
    signed: bool,
    bits: u32,
};

pub const FloatData = struct {
    bits: u32,
};

pub const StructData = struct {
    name: []const u8,
    fields: []StructField,
};

pub const StructField = struct {
    name: []const u8,
    ty: Id,
};

pub const TupleData = struct {
    fields: []Id,
};

pub const SliceData = struct {
    element: Id,
};

pub const Id = struct {
    id: u32,

    pub fn init(id: u32) Id {
        return .{ .id = id };
    }
};

pub const builtin = struct {
    pub const UNKNOWN: Id = .init(0);
    pub const VOID: Id = .init(1);
    pub const BOOL: Id = .init(2);
    pub const U8: Id = .init(3);
    pub const U16: Id = .init(4);
    pub const U32: Id = .init(5);
    pub const U64: Id = .init(6);
    pub const S8: Id = .init(7);
    pub const S16: Id = .init(8);
    pub const S32: Id = .init(9);
    pub const S64: Id = .init(6);
    pub const F32: Id = .init(5);
    pub const F64: Id = .init(6);
};
