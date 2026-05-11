# Haskell Logic Circuits & Vectronix Mainframe TUI

A bottom-up logic circuit simulator written in Haskell, paired with an interactive, vintage-inspired Terminal User Interface (TUI). 

## 🧠 Concept

This project is inspired by **[nand2tetris](https://www.nand2tetris.org/)** and serves as a practical demonstration of concepts from **CS-238L (Computer Logic Design)** and **CS-357 (Declarative Programming)**. 

The goal is to build a computer from first principles. The simulation implements addition arithmetic at the hardware level:
1. It begins with a custom, primitive 2-bit `NAND` gate.
2. From `NAND`, it derives `NOT`, `AND`, `OR`, and `XOR` gates.
3. These gates are wired together to form Half Adders and Full Adders.
4. The Full Adders are chained into an 8-bit Ripple-Carry Adder.
5. Finally, an ALU (Arithmetic Logic Unit) processes 8-bit data (`Byte`) representing signed (Two's Complement) and unsigned integers.

## 🖥️ The TUI: Vectronix Mainframe 2000

To visualize the logic circuits in action, the project includes a custom Terminal User Interface built using the `Layoutz` library. Designed to emulate the front panel of a vintage microcomputer or mainframe, the TUI allows you to interact with the simulated hardware in real-time.

### TUI Features
* **Animated POST Sequence:** The application boots with a simulated Power-On Self-Test (POST), checking memory and mounting the logic processor before handing over control to the front panel.
* **Interactive Hardware Registers:** Modify Register A and Register B using physical toggle switches or direct numeric entry. 
* **Real-time ALU Visualization:** Watch the ALU compute the sum of the registers. The green accumulation LEDs feature a simulated propagation delay (decay) to mimic electrical processing.
* **Diagnostic Logic Bus & State Monitor:** A live event log tracks bus interrupts and memory state shifts.
* **Dynamic Displays:** View register data in Decimal, Hexadecimal, or Octal format instantly. 

## 🚀 Installation & Usage

### Prerequisites
You will need the Haskell Toolchain ([GHC and Cabal](https://www.haskell.org/ghcup/)) installed on your system.

### Running the Simulator
Clone the repository and run the application using Cabal:

` ` `bash
git clone https://github.com/andrew-crowe-ok/haskell-logic-circuits.git
cd haskell-logic-circuits
cabal run logic-sim
` ` `
*Note: Ensure your terminal window is at least 80x40 characters, or the system's geometry checks will halt the boot process to prevent rendering errors.*

### Keyboard Controls
The front panel is fully keyboard-driven:
* **`[Tab]`** : Cycle focus forward (Register A Num -> Reg A Switches -> Reg B Num -> Reg B Switches).
* **`[b]`** : Cycle focus backward.
* **`[Space]`** : Toggle the currently focused hardware switch.
* **`[Up/Down]`** : Increment or decrement the decimal value of the focused register.
* **`[m]`** : Toggle the Arithmetic Mode (Unsigned Addition vs. Signed Two's Complement).
* **`[h]`** : Cycle the display base (Decimal -> Hexadecimal -> Octal).
* **`[q]`** : Safely power down the system and exit.

## 📂 Repository Structure

The project strictly separates the pure logic simulation from the side-effect-heavy UI:

* **`src/` (The Core Hardware Library):** Contains the pure functional logic simulator.
  * `Gates.hs`, `Circuits.hs`: The primitive gates and ripple-carry adders.
  * `Types.hs`, `Bit.hs`, `Byte.hs`: The data structures representing electrical states.
* **`app/` (The Application Front-End):** Contains the Vectronix Mainframe TUI.
  * `TUI.hs`: The view rendering, state models, and update loops.
  * `Main.hs`: Terminal preparation, geometry enforcement, and execution sequences.

## 🔮 Future Additions
* Bit-shifting operations and hardware multipliers.
* A simulated system clock to drive sequential circuits (Latches, Flip-Flops, Memory).