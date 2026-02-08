<h1 align="center" id="title">java-to-pyi</h1>

<p align="center"><img src="https://socialify.git.ci/n08i40k/java-to-pyi/image?description=1&amp;font=JetBrains+Mono&amp;language=1&amp;name=1&amp;owner=1&amp;pattern=Plus&amp;stargazers=1&amp;theme=Auto" alt="project-image"></p>

<p id="description">Java‑to‑PYI is a Rust tool that parses Java source into a preprocessed AST and generates Python stub files (.pyi) with type definitions only. It was originally designed to generate class declarations to provide accurate type hints in the Telegram codebase when developing Exteragram plugins.</p>

## Installation Steps

1. Clone this repo

```
git clone https://github.com/n08i40k/java-to-pyi
```

2. Compile and install

```
cargo install --path .
```

## Usage example

1. Clone OpenJDK source code (required for all use-cases, otherwise, non-promitive java types won't be resolved)

```
git clone https://github.com/openjdk/jdk
```

2. Remove sample file from OpenJDK source code

```
rm ./jdk/src/java.base/share/classes/java/lang/snippet-files/ProcessExamples.java
```

3. Clone Telegram for Android source code

```
git clone https://github.com/DrKLO/Telegram
```

4. Create directory for stub files

```
mkdir stubs
```

5. Start generator

```
java-to-pyi -i ./jdk/src/java.base/share/classes/java/ -i ./Telegram/TMessagesProj/src/main/java/ -o ./stubs
```

Where `-i` is directories with `.java` files and `-o` is output directory for generated stubs.
