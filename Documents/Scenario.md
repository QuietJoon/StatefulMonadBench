Scenario
====

The simulation takes three arguments

* Simulation Size: How many the simulation function recursive
* Target List: Which datum should be modified
* Instruction List
* Operand List

## State

The state have four major records in `Data`.

* Time
* Balance
* Status
* Map

### Time

Just a integer

### Balance

Just a integer

### Status

Just a integer

### Map

IntMap Int

## Definition

### Simulation Size

A single integer value.

### Target List

The simulation function could take one or two numbers from the list.

When the first number is not multiple of 4, then the target would be ID, Name or Status.
When the first number is multiple of 4, then the second number is a index of the target entry.

### Instruction

The simulation have only four instruction

* Set
* Mod
* Add
* Div

### Operand

The simulation function could take one number from the list as a operand for operation with the target value.
