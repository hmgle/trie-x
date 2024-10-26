const std = @import("std");

pub const Error = error{
    Null,
    Undefined,
};

pub const HitMeta = struct {
    word: []const u8,
    val: i32,
    offset: usize,
};

pub const Trie = struct {
    children: std.AutoHashMap(u8, *Trie),
    val: i32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) *Trie {
        const trie = allocator.create(Trie) catch unreachable;
        trie.* = .{
            .children = std.AutoHashMap(u8, *Trie).init(allocator),
            .val = 0,
            .allocator = allocator,
        };
        return trie;
    }

    pub fn deinit(self: *Trie) void {
        var it = self.children.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.children.deinit();
        self.allocator.destroy(self);
    }

    pub fn insert(self: *Trie, word: []const u8, val: i32) !void {
        if (word.len == 0) {
            self.val = val;
            return;
        }

        if (self.children.get(word[0])) |child| {
            try child.insert(word[1..], val);
        } else {
            const new_child = init(self.allocator);
            try self.children.put(word[0], new_child);
            try new_child.insert(word[1..], val);
        }
    }

    pub fn lookup(self: *Trie, word: []const u8) Error!i32 {
        if (word.len == 0) {
            return if (self.val != 0) self.val else Error.Null;
        }
        if (self.children.get(word[0])) |child| {
            return child.lookup(word[1..]);
        }
        return Error.Undefined;
    }

    pub fn traversal(self: *Trie, limit: ?usize) !std.StringHashMap(i32) {
        var ret = std.StringHashMap(i32).init(self.allocator);
        try self.traversalImpl("", &ret, limit);
        return ret;
    }

    fn traversalImpl(self: *Trie, prefix: []const u8, tmp_ret: *std.StringHashMap(i32), limit: ?usize) !void {
        if (limit) |l| {
            if (tmp_ret.count() >= l) {
                return;
            }
        }
        if (self.val != 0) {
            const cloned = try std.fmt.allocPrint(self.allocator, "{s}", .{prefix});
            try tmp_ret.put(cloned, self.val);
        }
        var it = self.children.iterator();
        while (it.next()) |entry| {
            var new_prefix = std.ArrayList(u8).init(self.allocator);
            defer new_prefix.deinit();

            try new_prefix.appendSlice(prefix);
            try new_prefix.append(entry.key_ptr.*);

            const new_prefix_slice = try new_prefix.toOwnedSlice();
            defer self.allocator.free(new_prefix_slice);
            try entry.value_ptr.*.traversalImpl(new_prefix_slice, tmp_ret, limit);
        }
    }

    pub fn expand(self: *Trie, prefix: []const u8, limit: ?usize) !?std.StringHashMap(i32) {
        return self.expandImpl(prefix, prefix, limit);
    }
    fn expandImpl(self: *Trie, origin_prefix: []const u8, prefix: []const u8, limit: ?usize) !?std.StringHashMap(i32) {
        if (prefix.len == 0) {
            var tmp_ret = std.StringHashMap(i32).init(self.allocator);

            defer {
                var it = tmp_ret.keyIterator();
                while (it.next()) |key| {
                    self.allocator.free(key.*);
                }
                tmp_ret.deinit();
            }

            const a: []const u8 = "";
            try self.traversalImpl(a, &tmp_ret, limit);
            var ret = std.StringHashMap(i32).init(self.allocator);
            var it = tmp_ret.iterator();
            while (it.next()) |entry| {
                const new_key = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ origin_prefix, entry.key_ptr.* });
                try ret.put(new_key, entry.value_ptr.*);
            }
            return ret;
        }
        if (self.children.get(prefix[0])) |child| {
            return child.expandImpl(origin_prefix, prefix[1..], limit);
        }
        return null;
    }

    pub fn scanContent(self: *Trie, content: []const u8) ![]HitMeta {
        var hits = std.ArrayList(HitMeta).init(self.allocator);
        defer hits.deinit();
        if (content.len == 0) {
            return hits.toOwnedSlice();
        }
        try self.scanContentImpl(content[0..1], content[1..], 0, &hits, 0);
        return hits.toOwnedSlice();
    }

    fn scanContentImpl(
        self: *Trie,
        k: []const u8,
        remain: []const u8,
        offset_remain: usize,
        hits: *std.ArrayList(HitMeta),
        offset: usize,
    ) !void {
        if (remain.len == 0) {
            const val = self.lookup(k) catch {
                return;
            };
            const cloned = try std.fmt.allocPrint(self.allocator, "{s}", .{k});
            try hits.append(HitMeta{
                .word = cloned,
                .val = val,
                .offset = offset,
            });
            return;
        }
        if (remain.len > offset_remain) {
            const val = self.lookup(k) catch |err| switch (err) {
                Error.Undefined => {
                    return try self.scanContentImpl(remain[0..1], remain[1..], 0, hits, offset + 1);
                },
                Error.Null => {
                    const new_k = try std.fmt.allocPrint(self.allocator, "{s}{c}", .{ k, remain[offset_remain] });
                    defer self.allocator.free(new_k);
                    return try self.scanContentImpl(new_k, remain, offset_remain + 1, hits, offset);
                },
            };
            const cloned = try std.fmt.allocPrint(self.allocator, "{s}", .{k});
            try hits.append(HitMeta{
                .word = cloned,
                .val = val,
                .offset = offset,
            });
            const new_k = try std.fmt.allocPrint(self.allocator, "{s}{c}", .{ k, remain[offset_remain] });
            defer self.allocator.free(new_k);
            try self.scanContentImpl(new_k, remain, offset_remain + 1, hits, offset);
        } else {
            const val = self.lookup(k) catch |err| switch (err) {
                Error.Null, Error.Undefined => {
                    try self.scanContentImpl(remain[0..1], remain[1..], 0, hits, offset + 1);
                    return;
                },
            };
            const cloned = try std.fmt.allocPrint(self.allocator, "{s}", .{k});
            try hits.append(HitMeta{
                .word = cloned,
                .val = val,
                .offset = offset,
            });
            try self.scanContentImpl(remain[0..1], remain[1..], 0, hits, offset + 1);
        }
    }
};

fn buildTrieFromPath(file_path: []const u8, allocator: std.mem.Allocator) !*Trie {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allcator = arena.allocator();

    var trie = Trie.init(allocator);

    while (try in_stream.readUntilDelimiterOrEofAlloc(allcator, '\n', 1024)) |line| {
        try trie.insert(line, 1);
    }
    return trie;
}

test "trie" {
    const allocator = std.testing.allocator;
    const en_word_trie = try buildTrieFromPath("../data/google-10000-english.txt", allocator);
    defer en_word_trie.deinit();
    const sen_word_trie = try buildTrieFromPath("../data/keyword-pool.txt", allocator);
    defer sen_word_trie.deinit();
    const bad_word_trie = try buildTrieFromPath("../data/bad-words-en.txt", allocator);
    defer bad_word_trie.deinit();

    var expand_tr = en_word_trie.expand("qu", 10) catch unreachable;
    defer {
        var eit = expand_tr.?.keyIterator();
        while (eit.next()) |key| {
            allocator.free(key.*);
        }
        expand_tr.?.deinit();
    }
    var eit = expand_tr.?.iterator();
    std.debug.print("words of begin with `qu`, limit 10: \n", .{});
    while (eit.next()) |entry| {
        std.debug.print("{s}\n", .{entry.key_ptr.*});
    }

    const content = "近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员";
    const hits = try sen_word_trie.scanContent(content);
    defer {
        for (hits) |hit| {
            allocator.free(hit.word);
        }
        allocator.free(hits);
    }
    std.debug.print("\n*sensitive words*: {s}\n", .{content});
    for (hits) |hit| {
        std.debug.print("word: {s}, val: {} offset: {}\n", .{ hit.word, hit.val, hit.offset });
    }

    const content_en = "What does lemon party mean? In a brief filed ahead of oral arguments, the state argued that the law does not ban 18- to 20-year-old women from erotic dancing, which is protected under the First Amendment.";
    const hits_en = try bad_word_trie.scanContent(content_en);
    defer {
        for (hits_en) |hit| {
            allocator.free(hit.word);
        }
        allocator.free(hits_en);
    }
    std.debug.print("\n*sensitive words*: {s}\n", .{content_en});
    for (hits_en) |hit| {
        std.debug.print("word: {s}, val: {} offset: {}\n", .{ hit.word, hit.val, hit.offset });
    }
}
