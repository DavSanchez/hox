package main

import (
	"fmt"
)

type Expr struct {
	Name string
}

func main() {
	fmt.Println("=== Go Map Key Behavior Demo ===\n")

	// Scenario 1: Using Struct Values as Keys
	// In Go, structs are comparable by value (content).
	fmt.Println("--- Scenario 1: Struct Values as Keys ---")
	mapByValue := make(map[Expr]int)

	val1 := Expr{Name: "a"}
	val2 := Expr{Name: "a"}

	fmt.Printf("val1: %v\n", val1)
	fmt.Printf("val2: %v\n", val2)
	fmt.Printf("Are they equal? (val1 == val2): %v\n", val1 == val2)

	mapByValue[val1] = 1
	fmt.Println("Set map[val1] = 1")

	// This will OVERWRITE the previous entry because val1 == val2
	mapByValue[val2] = 2
	fmt.Println("Set map[val2] = 2")

	fmt.Printf("Value for val1: %d\n", mapByValue[val1]) // Will be 2
	fmt.Printf("Value for val2: %d\n", mapByValue[val2]) // Will be 2
	fmt.Println()

	// Scenario 2: Using Pointers as Keys
	// Pointers are comparable by address (reference identity).
	// This mimics the Java behavior we saw.
	fmt.Println("--- Scenario 2: Pointers as Keys ---")
	mapByPointer := make(map[*Expr]int)

	ptr1 := &Expr{Name: "a"}
	ptr2 := &Expr{Name: "a"}

	fmt.Printf("ptr1: %p (Name: %s)\n", ptr1, ptr1.Name)
	fmt.Printf("ptr2: %p (Name: %s)\n", ptr2, ptr2.Name)
	fmt.Printf("Are they equal? (ptr1 == ptr2): %v\n", ptr1 == ptr2)

	mapByPointer[ptr1] = 1
	fmt.Println("Set map[ptr1] = 1")

	// This will create a NEW entry because ptr1 != ptr2
	mapByPointer[ptr2] = 42
	fmt.Println("Set map[ptr2] = 42")

	fmt.Printf("Value for ptr1: %d\n", mapByPointer[ptr1]) // Will be 1
	fmt.Printf("Value for ptr2: %d\n", mapByPointer[ptr2]) // Will be 42
}
