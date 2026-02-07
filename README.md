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
cargo install
```

## Usage example

1. Clone Telegram for Android source code

```
git clone https://github.com/DrKLO/Telegram
```

2. Replace `@interface` by `interface` in all files, only if you are want to convert code from repo provided in this example

```
find ./Telegram/TMessagesProj/src/main/java/ -type f \( -name '*.java' \) -exec sed -i 's/@interface/interface/g' {} +
```

3. Start generator

```
java-to-pyi -i ./Telegram/TMessagesProj/src/main/java -o ./
```

Where `-i` is root directory of java source code and `-o` is output directory for generated package stubs.
