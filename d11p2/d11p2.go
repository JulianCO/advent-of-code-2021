package main

import (
	"fmt"
	"io/fs"
	"os"
)

type OctopusField struct {
	Energies   [100]int
	HasFlashed [100]bool
	FlashCount int
}

func main() {
	local_dir := os.DirFS(".")
	fileContents, err := fs.ReadFile(local_dir, "input")
	var energies [100]int
	if err != nil {
		fmt.Println(err)
		return
	}

	j := 0
	for _, v := range fileContents {
		if v-'0' >= 0 && v-'0' < 10 {
			energies[j] = int(v - '0')
			j++
		}
	}

	var of OctopusField
	of.Energies = energies

	printEnergies(energies[:])
	printEnergies(of.Energies[:])

	i := 0
	for !allFlashed(&of) {
		step(&of)
		i++
	}

	printEnergies(of.Energies[:])
	fmt.Println(of.FlashCount)
	fmt.Println(i)
}

func allFlashed(of *OctopusField) bool {
	for _, v := range of.Energies {
		if v != 0 {
			return false
		}
	}
	return true
}

func increaseEnergy(of *OctopusField, i int) {
	if i < 0 || i >= 100 {
		return
	}
	of.Energies[i] += 1
	if of.HasFlashed[i] {
		return
	}
	if of.Energies[i] >= 10 {
		of.FlashCount += 1
		of.HasFlashed[i] = true
		if i%10 != 0 {
			increaseEnergy(of, i-11)
			increaseEnergy(of, i-1)
			increaseEnergy(of, i+9)
		}
		increaseEnergy(of, i-10)
		increaseEnergy(of, i+10)
		if i%10 != 9 {
			increaseEnergy(of, i-9)
			increaseEnergy(of, i+1)
			increaseEnergy(of, i+11)
		}
	}
}

func resetFlashed(of *OctopusField) {
	for i := range of.Energies {
		if of.HasFlashed[i] {
			of.HasFlashed[i] = false
			of.Energies[i] = 0
		}
	}
}

func step(of *OctopusField) {
	for i := range of.Energies {
		increaseEnergy(of, i)
	}
	resetFlashed(of)
}

func printEnergies(e []int) {
	for i := 0; i < 10; i++ {
		fmt.Println(e[10*i : 10*i+10])
	}
	fmt.Println("")
}
