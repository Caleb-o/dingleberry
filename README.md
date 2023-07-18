# Dingleberry

Dingleberry is a cringe language written in Rust. It is a dynamic, garbage collected language. It is supposed to have a similar syntax feel to Rust, to make it simpler to switch between it and Rust.

Its main purpose is for testing and tweaking the garbage collector for other projects, but could still be used on its own to do small and basic tasks.

[Hello World](./examples/hello_world.dingle)

```rs
fn hello_world(name) {
    print('Hello, ', 'World! Hello, ', name);
}

hello_world('Bob');
```

[Modules](./examples/modules.dingle)

```rs
module Foo {
    module Bar {
        fn other_func {
            print('Other func');
        }
    }

    fn foo {
        this.bar('Dave');
    }

    fn bar(name) {
        print('Hello ', name, '!');
        this.Bar.other_func();
    }
}

call_fn(Foo.foo);

fn call_fn(f) = f();
```

[Structs](./examples/structs.dingle)

```rs
module Stuff {
    struct Foo {
        fn hello {
            print('Hello!');
        }
    }

    struct Bar {
        fn hello {
            print('Hello 2!');
        }
    }

    fn call_hello(struct_) {
        struct_.hello();
    }
}

Stuff.call_hello(Stuff.Foo());
Stuff.call_hello(Stuff.Bar());
```

[Struct Constructor](./examples/struct_constructor.dingle)

```rs
# The constructor is done with an argument list, rather than a function
struct Foo(a, b) {
    let a, b;

    fn my_fields {
        print('My fields, a: ', this.a, ', b: ', this.b);
    }
}

# Instantiate an instance with a constructor
Foo(10, 20).my_fields();
# Can call any function from the type itself
Foo.my_fields();
```
