package main

import (
	"fmt"
	"io/fs"
	"os"
	"regexp"
)

func solution(graph *map[string][]string) int {
	smallCave := make(map[string]bool)
	for cave := range *graph {
		smallCave[cave], _ = regexp.Match("[a-z]+", []byte(cave))
	}
	visited := make(map[string]bool)

	return countPaths("start", graph, &smallCave, &visited)
}

func countPaths(start string, graph *map[string][]string, smallCave *map[string]bool, visited *map[string]bool) int {
	if (*smallCave)[start] && (*visited)[start] {
		return 0
	}
	if start == "end" {
		return 1
	}
	(*visited)[start] = true

	total := 0
	for _, neighbor := range (*graph)[start] {
		total += countPaths(neighbor, graph, smallCave, visited)
	}
	(*visited)[start] = false
	return total
}

func main() {
	local_dir := os.DirFS(".")
	input, err := fs.ReadFile(local_dir, "input")
	r := regexp.MustCompile("([A-Za-z]+)-([A-Za-z]+)\n")
	if err != nil {
		fmt.Println("Error opening file", err)
		return
	}
	graph := make(map[string][]string)

	fmt.Println(string(input))

	parsed := r.FindAllStringSubmatch(string(input), -1)

	for _, v := range parsed {
		fmt.Println("Linking", v[1], "and", v[2])
		graph[v[1]] = append(graph[v[1]], v[2])
		graph[v[2]] = append(graph[v[2]], v[1])
	}

	fmt.Println(graph["start"])
	fmt.Println(solution(&graph))
}
