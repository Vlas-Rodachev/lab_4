open System

type BinaryTree =
    | Node of Double * BinaryTree * BinaryTree
    | Empty


let rec vvod_znach_i (mes:string) = 
    printf "%s: " mes  
    let str = Console.ReadLine()  
    match Int32.TryParse(str) with  
    | (true, value) -> Some value  
    | _ ->   
        printfn "Неверный ввод!!!!!"
        vvod_znach_i(mes)

let rec vvod_znach_f (mes:string) = 
    printf "%s: " mes  
    let str = Console.ReadLine()  
    match Double.TryParse(str) with  
    | (true, value) -> Some value  
    | _ ->   
        printfn "Неверный ввод!!!!!"
        vvod_znach_f(mes)

// вывод дерева
let rec printTree tree otst =
    match tree with
    | Node (data, left, right)
        -> let otdt1 = otst + 2
           for i in 0..otst do printf " "
           printfn "%f" data
           printTree left otdt1
           printTree right otdt1
    | Empty -> ()

// добавление 1 элемента
let rec add_el_tree tree el = 
    match tree with
    | Node (data, left, right) ->
        if data > el then  Node (data, add_el_tree left el, right)
        else Node (data, left, add_el_tree right el)
    | Empty -> Node (el, Empty, Empty)

// заполнение вручную
let rec zap_tree tree =
    let x = vvod_znach_f("Введите значение").Value
    match x with
    | 0.0 -> tree
    | _ -> let tree1 = add_el_tree tree x
           zap_tree tree1

let rnd = new Random()
let rec zap_tree_rand tree n =
    let x = double(rnd.Next(100)) + rnd.NextDouble()
    match n with
    | 0 -> tree
    | _ -> let tree1 = add_el_tree tree x
           zap_tree_rand tree1 (n - 1)


let fun1 x = 
    round(x)

// задание 1
let rec tree_map f1 tree tree_new = 
    match tree with
    | Node (data, left, right)
        -> let x = f1 data 
           let t = add_el_tree tree_new x
           let t1 = tree_map f1 left t
           tree_map f1 right t1
    | Empty -> tree_new


let fun2 x y z = 
    if x = y then z + 1
    else z

// задание 2
let rec tree_fold f2 tree el = 
    match tree with
    | Node (data, left, right)
        -> let x = f2 data el 0
           x + (tree_fold f2 left el) + (tree_fold f2 right el)
    | Empty -> 0

let a = ""
// функция для получения дерева на выбор, вручную или случайным образом
let rec get_tree () = 
    printfn $"{a}"
    printfn "%A" "1 - Ввести дерево вручную"
    printfn "%A" "2 - Сгенерировать автоматически"
    match vvod_znach_i("Ведите номер задания").Value with
    | 1 -> 
        printfn $"{a}"
        zap_tree Empty
    | 2 -> 
        let n = vvod_znach_i("Ведите количество элементов").Value
        printfn $"{a}"
        zap_tree_rand Empty n
    | _ -> 
        printfn $"{a}"
        get_tree ()


let rec fuuuuuu () = 
    printfn "1 - Округлить все элемнты дерева"
    printfn "2 - Поиск количества элементов равных данному"
    printfn "0 - Выход"
    match vvod_znach_i("Ведите номер задания").Value with
    | 1 -> 
        let li = get_tree ()
        printTree li 0
        printfn $"{a}"
        let li1 = tree_map fun1 li Empty
        printfn $"{a}"
        printfn $"{a}"
        printTree li1 0
        fuuuuuu ()
    | 2 -> 
        let li = get_tree ()
        printTree li 0
        let c = vvod_znach_f("Введите значение искомого элемента").Value
        printfn $"{a}"
        let li1 = tree_fold fun2 li c
        printfn "%d" li1
        printfn $"{a}"
        printfn $"{a}"
        fuuuuuu ()
    | 0 -> ()
    | _ -> 
        printfn $"{a}"
        printfn $"{a}"
        fuuuuuu ()

fuuuuuu()

