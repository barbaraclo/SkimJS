//Teste 1: array
// var listNumber = [1,2,3];
// var listString = ["plc", "melhorcadeira"];
// var listListNumber = [[1,1],[2,2],[3,3]];
// var lista1 = [12,1,4,9,32,22,55,1,9,0,5];
// var lista2 = ["k", "r", "b"];
// var a = 2;
// var b = 1;
// var k = ["a","b","c"];
// var y =[1,1,2];
// var w = ["1,2","b"];
// var c = k.head();
// var d = k.tail();
// var e = k.concat(y);
// var f = k.len();
// c;


// //Teste 2: Function
// function tamanhoList (listNumber){
//    var result = listNumber.len();
//    return result + 1;
// }

// function c (ls, xs){
// var c = lista1.len();
// 	return ls.concat(xs);
// }
function isPrime(x) {
	var count = 0;
	var i = 1;
	for ( ; i <= x; i = i + 1){
		if (x % i == 0){ 
			count = count + 1;
		}
	}
	// if(count == 2){
	// 	return true;
	// }
	return false;
}
isPrime(11);


// //Teste 3: If-else
// function parImpar (){
// 	var x = 5;
// 	if(x%2 == 0){
// 		return "par";
// 	}else{
// 		return "impar";
// 	}
// }

// //Teste 4: Concatenação
// function conca (lista1, lista2){
// var c = lista1.len();
// 	return lista1.concat(lista2);
// }

// //Teste 5: While
// var x = 10;
// while (x > 0) {
// 	x = x - 1;
// }

// //Teste 6: For
// var x = 10;
// for (;x>0;){
// 	x = x -1;
// }

// //Teste 7: Break
// var y = 24;
// while (y>0){
// 	y = y + 1;
// 	if (y == 30){
// 		break;
// 	}
// }

// //Teste 8: Head
// function funcaoHead (listNumber){
// 	var cabeca = listNumber.head();
// 	return cabeca;
// }

// //Teste 9: Tail
// function funcaoTail (listNumber){
// 	var resto = listNumber.tail();
// 	return resto;
// }

// //Teste 10: Concatenação
// function funcaoConcat (listNumber, listString){
// 	return listNumber.concat(listString);
// }

// //Teste 11: Do-While
// do {
// 	x = a;
// 	a = 1;
// }
// while (b >a)

// //Teste 12: Break
// if (a >b){
// 	break;
// }

// //Teste 13: Switch
// switch (x) {

// 	case 1:
// 		y = [2];
// 		break;

// 	case 2:
// 		y = [3];
// 		break;

// }

// //Teste 14: For
// for (; x < 15;) {
// 	x = x+1;
// }

// //Teste 15: Break
// x = 20;
// for (var i = 0; i<3 ; i = i + 1){
// 	x = x+1;
// 	break;
	
// }





