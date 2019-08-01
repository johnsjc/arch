## Author: Jc J
## Date: July 26, 2019
##
## Receives a list of numbers from stdin and sorts them in ascending order using different sorting algorithms.
##
## Use sort.sh to generate random inputs.
##
## Example run:
##
## $ ./sort.sh 5
##
## Input: [2, 4, 1, 3, 5]
##
## Sorting using bubble sort..
##
## [DEBUG] Pass: 1
## 		[DEBUG] Sort: [2, 4, 1, 3, 5]
## 		[DEBUG] Swap: [4] | [1]
## 		[DEBUG] Result: [2, 1, 4, 3, 5]
##
## 		[DEBUG] Sort: [2, 1, 4, 3, 5]
## 		[DEBUG] Swap: [4] | [3]
## 		[DEBUG] Result: [2, 1, 3, 4, 5]
##
## [DEBUG] Pass: 2
## 		[DEBUG] Sort: [2, 1, 3, 4, 5]
## 		[DEBUG] Swap: [2] | [1]
## 		[DEBUG] Result: [1, 2, 3, 4, 5]
##
## [DEBUG] Pass: 3
## [DEBUG] No swaps necessary.
##
## Sorted: [1, 2, 3, 4, 5]
##
####################
## 
## Sorting using a recursive merge sort algorithm...
## 
## [DEBUG] Sort: [2, 4, 1, 3, 5]
## [DEBUG] Split: [2, 4] | [1, 3, 5]
##         [DEBUG] Sort: [2, 4]
##         [DEBUG] Split: [2] | [4]
##                 [DEBUG] Sort: [2]
##                 [DEBUG] Sort: [4]
##         [DEBUG] Sorted left half: [2]
##         [DEBUG] Sorted right half: [4]
##         [DEBUG] Merged result: [2, 4]
##         [DEBUG] Sort: [1, 3, 5]
##         [DEBUG] Split: [1] | [3, 5]
##                 [DEBUG] Sort: [1]
##                 [DEBUG] Sort: [3, 5]
##                 [DEBUG] Split: [3] | [5]
##                         [DEBUG] Sort: [3]
##                         [DEBUG] Sort: [5]
##                 [DEBUG] Sorted left half: [3]
##                 [DEBUG] Sorted right half: [5]
##                 [DEBUG] Merged result: [3, 5]
##         [DEBUG] Sorted left half: [1]
##         [DEBUG] Sorted right half: [3, 5]
##         [DEBUG] Merged result: [1, 3, 5]
## [DEBUG] Sorted left half: [2, 4]
## [DEBUG] Sorted right half: [1, 3, 5]
## [DEBUG] Merged result: [1, 2, 3, 4, 5]
## 
## Sorted: [1, 2, 3, 4, 5]
## 
####################
## 
## Sorting using an iterative merge sort algorithm...
## 
## [DEBUG] Size: 1 
## 		[DEBUG] Split: [2] | [4]
## 		[DEBUG] Merged result: [2, 4]
##
## 		[DEBUG] Split: [1] | [3]
## 		[DEBUG] Merged result: [1, 3]
##
## 		[DEBUG] Split: [5]
## 		[DEBUG] Merged result: [5]
##
## [DEBUG] Size: 2
## 		[DEBUG] Split: [2, 4] | [1, 3]
## 		[DEBUG] Merged result: [1, 2, 3, 4]
##
## 		[DEBUG] Split: [5]
## 		[DEBUG] Merged result: [5]
##
## [DEBUG] Size: 4 
## 		[DEBUG] Split: [1, 2, 3, 4] | [5]
## 		[DEBUG] Merged result: [1, 2, 3, 4, 5]
## 
## Sorted: [1, 2, 3, 4, 5]
##

.data

## A list has the following structure:
##       typedef struct _list_t {
##           int     n_elements;
##           int     *elements;
##       } list_t;
list:				.space 8		# the list that will be sorted
unsorted:			.space 8		# a copy of the unsorted list

input_msg:			.asciiz "Input: "
sorted_msg:			.asciiz "Sorted: "
bubble_msg:			.asciiz "\nSorting using bubble sort..\n"
recursive_msg:		.asciiz "\nSorting using a recursive merge sort algorithm...\n"
iterative_msg:      .asciiz "\nSorting using an iterative merge sort algorithm...\n"

.text

## main
##
## 1. 	Read the input, which consists of the following elements:
##			a. The number of integers to follow
##			b. Each integer on its own line
## 2. 	Dynamically create the list in memory.
## 3. 	Sort the list using a recursive merge sort algorithm.
## 4.	Sort the list using an iterative merge sort algorithm.
##
## Register usage:
##		s0:		list.n_elements, scratch space
##		s1:		list.elements
##
main:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
            
	m_create_list:
	
		la			$s0, unsorted								# s0: unsorted (unmutable)
		la			$s1, list									# s1: list (will be modified by algorithms)
	
		li			$v0, 5										# read the first line of the input (n_elements)
		syscall													# and store in the first word of the list structures.
		sw			$v0, 0($s0)
		sw			$v0, 0($s1)
			
		mul			$a0, $v0, 4									# allocate n_elements * 4 bytes on the heap for unsorted.elements
		li			$v0, 9										# and store the address in the second word
		syscall
		sw			$v0, 4($s0)
		
		lw			$a0, 0($s1)									# allocate the same amount of space for list.elements
		mul			$a0, $a0, 4									# and store the address in the second word
		li			$v0, 9										
		syscall
		sw			$v0, 4($s1)
			
		lw			$s2, 0($s0)									# s2: n_elements

		lw			$s0, 4($s0)									# s0: unsorted.elements
		lw			$s1, 4($s1)									# s1: list.elements
				
			
	m_populate_list:
	
		beqz		$s2, m_populate_list_end					# while we still have integers to read
			
		li			$v0, 5										# read the next integer and store it in unsorted.elements
		syscall
		sw			$v0, ($s0)										
			
		addu		$s0, $s0, 4									# increment the list.elements pointer
		subi		$s2, $s2, 1									# i--
		b			m_populate_list
		
	m_populate_list_end:

		li			$a0, 0xA									# print "\nInput: [2, 3, 1..]\n"
		li			$v0, 11
		syscall
		la			$a0, input_msg
		li			$v0, 4
		syscall
		la			$a0, unsorted
		jal			util.print_list
		li			$a0, 0xA
		li			$v0, 11
		syscall
		
	m_sort_bubble:
	
		la			$a0, bubble_msg								# print "Sorting using bubble sort..\n"
		li			$v0, 4
		syscall
		
		la			$a0, sort.bubble_sort
		jal			sort
		jal			print_sep
			
	m_sort_recursive:
	
		la			$a0, recursive_msg							# print "Sorting using a recursive merge sort algorithm..\n"
		li			$v0, 4
		syscall
		
		la			$a0, sort.recursive_merge_sort
		jal			sort
		jal			print_sep
	
	m_sort_iterative:	
				
		la			$a0, iterative_msg							# print "Sorting using an iterative merge sort algorithm.."
		li			$v0, 4
		syscall
			
		la			$a0, sort.iterative_merge_sort
		jal			sort
	
	exit:
			
		li			$v0, 10
		syscall
			
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
			
		jr			$ra
## end of main

## sort : void
##
## sort the list using the given function
##
## Arguments:
##		a0:		address of the sort function
##
## Register usage:
##		s0:		address of the sort function
##
sort:
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		sw			$s0, 24($sp)
		
		move		$s0, $a0									# s0: address of sorting algorithm
		
		la			$a0, unsorted								# make a copy of the unsorted list to work with
		la			$a1, list
		jal			util.clone_list
		
		la			$a0, list
		jalr		$s0											# execute the sort function				
		
        la      	$a0, sorted_msg                         	# print "Sorted: [1, 2, 3..]\n"
        li      	$v0, 4
        syscall
        la      	$a0, list                            
        jal     	util.print_list
        li      	$a0, 0xA                                
        li      	$v0, 11
        syscall

		lw			$ra, 28($sp)
		lw			$s0, 24($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of sort


## print_sep : void
##
## prints a separator
##
print_sep:
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		li			$a0, 0xA									# print a separator
		li			$v0, 11
		syscall
		li			$a0, 0x23									
		li			$a1, 20
		jal			util.print_char_n_times
		li			$a0, 0xA
		li			$v0, 11
		syscall			
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of print_sep


.include "util.s"
.include "sort.s"
.include "debug.s"
