---
id: file-io
title: File Management
sidebar_position: 1
---

# File Management

The language provides native functions to manipulate files.

## `open(path, mode)`

Opens a file.

- **Arguments**:
  - `path`: String (e.g., `"file.txt"`).
  - `mode`: String (`"r"` read, `"w"` write/overwrite, `"a"` append).
- **Returns**: A file handle (`File`).

```clad
variable f open("test.txt", "w")
```

## `write(file, content)`

Writes to an open file.

```clad
write(f, "Hello CLaD")
```

## `read(file)`

Reads the entire content of a file.

```clad
variable f2 open("test.txt", "r")
variable content read(f2)
print(content)
```

## `close(file)`

Closes the file (releases the resource).

```clad
close(f)
close(f2)
```
