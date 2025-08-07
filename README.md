# 🏛️ Matrix Discovery

![Matrix Discovery - Event Log Analyzer](https://img.shields.io/badge/Matrix%20Discovery-Event%20Log%20Analyzer-blue)
![Rust](https://img.shields.io/badge/Built%20with-Rust-orange?logo=rust)
![License](https://img.shields.io/badge/License-MIT-blue)

Matrix Discovery is a Rust-based web application for processing and analyzing event logs in the XES format. It provides an interface to import, process, and generate XES files.

You can find a live demo of the application at [https://anonymoushlmnop.github.io/matrix-discovery/](https://anonymoushlmnop.github.io/matrix-discovery/)

## ✨ Features

- **Import XES files** for comprehensive analysis
- **Convert text input to XES format** (ideal for testing purposes)
- **Generate adjacency matrices** and other key metrics from event logs
- **Interactive web interface** for visualizing process mining results

## 🚀 Repository Overview
```
├── sample-data
│   ├── synthetic-log/
│   └── synthetic-logs-noise/
└── src
    ├── dependency_types
        ├── existential.rs
        └── temporal.rs
```

- In `temporal.rs` you can find the `check_temporal_dependency()` function. This is
where the temporal dependency is discovered for a given activity pair, set of traces
and threshold.
- In `existential.rs` you can find the `check_existential_dependency()` function. This is
where the existential dependency is discovered for a given activity pair, set of traces
and threshold.
- In the `sample-data/` folder you can find the manually generated synthetic logs that were
used for evaluation.

## 🔧 Prerequisites

- **Rust and Cargo**: Install from [rustup.rs](https://rustup.rs/)
- **[Trunk](https://trunkrs.dev/)**: WASM web application bundler for Rust
  ```sh
  cargo install trunk
  rustup target add wasm32-unknown-unknown
  ```
- **Nix** (optional): Development shell available with:
  ```sh
  nix-shell
  ```

## 🚀 Getting Started

### 1. Clone the repository

```sh
git clone https://github.com/anonymoushlmnop/matrix-discovery.git && cd matrix-discovery
```

### 2. Start the web application

```sh
trunk serve
```

### 3. Access the application

Open your web browser and navigate to [http://localhost:8080/matrix-discovery](http://localhost:8080/matrix-discovery)

> **Tips:** 
> - Use `trunk serve --open` to automatically open in your default browser
> - Specify a custom port with `trunk serve --port 1234`

## 📋 Usage Guide

### Importing XES Files
1. Click on the "Import XES" button
2. Select an XES file from your system
3. The file will be processed automatically

**Note**:
On the bottom left you can find a field in which you can adjust the `Temporal threshold` as well as the `Existential threshold`.
The thresholds are used for setting a threshold for the number of traces that should be considered for a temporal dependency or existential dependency.
For instance, if you set the threshold to `0.7` it would mean that you expect at least 70% of the traces to be true for a
temporal dependency or existential dependency to be considered valid. This is mainly useful for dealing with noisy event logs.
The thresholds should be set before importing the XES file.

### Analyzing Results
After importing, the application will:
- Generate an adjacency matrix based on event traces
- Display the matrix directly in the interface

## 🧪 Evaluation

The project includes evaluation tools for testing dependencies in event logs. Sample event logs and their expected dependencies are provided for testing and validation.

### How to evaluate with custom data
First you will need an adjacency matrix with predefined dependencies, that you know is correct, and the corresponding event log. After that you should go to the evaluation page, which you can do by either clicking on the `Evaluation` button on the bottom right of the main page, or by simply navigating to [https://anonymoushlmnop.github.io/matrix-discovery/evaluation](https://anonymoushlmnop.github.io/matrix-discovery/evaluation). There you will be able to first input all of your relationships, one by one, and then import an event log. After you're done doing that, click on `Evaluate Dependencies` and you will get the results displayed, which will show you how many temporal dependencies were correctly identified, and same for the existential dependencies.

Additionally you can find the adjacency matrices used for the evaluation inside of the `evaluation.rs` file. The numbers correspond to the event log numbers found in `sample-data/`.

### Sample Data
All sample data can be found in the `sample-data/` directory:
- `synthetic-log/`: Contains synthetic event logs (event_log_01.xes through event_log_11.xes)
- `synthetic-log-noise/`: Contains the same logs but polluted with some noise (used for evaluating the accuracy of the algorithm)


## 🧩 Core Dependencies

| Dependency | Purpose |
|------------|---------|
| [Yew](https://yew.rs/) | Modern Rust framework for front-end web apps using WebAssembly |
| [wasm-bindgen](https://rustwasm.github.io/wasm-bindgen/) | High-level interactions between Rust and JavaScript |
| [web-sys](https://rustwasm.github.io/wasm-bindgen/web-sys/) | Bindings for Web APIs |
| [process_mining](https://crates.io/crates/process_mining) | Process mining library for Rust |

## 📜 License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## 👏 Acknowledgments

- Thanks to the contributors of the Rust and Yew communities for their support and tools
