import System.IO
import System.Directory

type Servico = (Int, String, Int, Float)


servicoGetCod :: Servico -> Int
servicoGetCod (i,_,_,_) = i

servicoGetDesc :: Servico -> String
servicoGetDesc (_,i,_,_) = i

servicoGetPrest :: Servico -> Int
servicoGetPrest (_,_,i,_) = i

servicoGetPreco :: Servico -> Float
servicoGetPreco (_,_,_,i) = i

newServico :: IO()
newServico = do
    print ("[NOVO SERVICO]")
    print ("Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print ("Nome:")
    nome <- getLine
    print ("Quantidade de Prestacoes:")
    quantidadeString <- getLine
    let quantidade = (read quantidadeString :: Int)
    print ("Preco por Hora:")
    precoString <- getLine
    let preco = (read precoString :: Float)
    saveServico(codigo, nome, quantidade, preco)

saveServico :: Servico -> IO()
saveServico s = do
    out <- openFile "servico.db" AppendMode
    hPutStrLn out (show s)
    hClose out

deleteServico :: IO()
deleteServico = do
    inp <- openFile "servico.db" ReadMode
    openServico inp
    print("ID do servico a ser removido:")
    deleteidstr <- getLine
    let deleteid = (read deleteidstr :: Int)
    hClose inp
    renameFile "servico.db" "oldservico.db"
    inp <- openFile "oldservico.db" ReadMode
    -- renameFile "servico.db" "oldservico.db"
    out <- openFile "servico.db" AppendMode
    deleteServicoFromID deleteid out inp
    hClose inp
    hClose out
    removeFile "oldservico.db"

deleteServicoFromID :: Int -> Handle -> Handle -> IO()
deleteServicoFromID id out inp =
    do endoffile <- hIsEOF inp
       if endoffile
            then return ()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Servico)
                if (servicoGetCod select)==id
                    then print("achou")
                    else hPutStrLn out (show select)
                deleteServicoFromID id out inp

openServico :: Handle -> IO()
openServico inp = 
    do inpeof <- hIsEOF inp
       if inpeof
            then return ()
            else do
                inpString <- hGetLine inp
                print (inpString)
                openServico inp

changeServico :: IO()
changeServico = do
    inp <- openFile "servico.db" ReadMode
    openServico inp
    print("ID do servico a ser alterado:")
    changeidstr <- getLine
    let changeid = (read changeidstr :: Int)
    hClose inp
    inp <- openFile "servico.db" ReadMode
    selectServicoFromID changeid inp
    hClose inp

selectServicoFromID :: Int -> Handle -> IO()
selectServicoFromID id inp =
    do endoffile <- hIsEOF inp
       if endoffile
            then return ()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Servico)
                if (servicoGetCod select)==id
                    then changeSelectedServico select inp
                    else selectServicoFromID id inp

changeSelectedServico :: Servico -> Handle -> IO()
changeSelectedServico s h = do
    print(s)
    let id = servicoGetCod s
    let desc = servicoGetDesc s
    let prest = servicoGetPrest s
    let preco = servicoGetPreco s
    print("Escolha o campo para alterar:")
    print("1 - ID")
    print("2 - Descricao")
    print("3 - Quantidade de Prestacoes")
    print("4 - Preco por Hora")
    changefieldstr <- getLine
    let changefield = (read changefieldstr :: Int)
    print("Novo valor do campo:")
    if (changefield==1)
        then do changevaluestr <- getLine
        let changevalue = (read changevaluestr :: Int)
        print(changevalue)
        else print("lol")

type Cliente = (Int, String, String, Int, String)

clienteGetCod :: Cliente -> Int
clienteGetCod (i,_,_,_,_) = i

newCliente :: IO()
newCliente = do
    print ("[NOVO CLIENTE]")
    print ("Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print ("Nome:")
    nome <- getLine
    print ("Cidade:")
    cidade <- getLine
    print ("Idade:")
    idadeString <- getLine
    let idade = (read idadeString :: Int)
    print ("Genero (M/F):")
    genero <- getLine
    saveCliente(codigo, nome, cidade, idade, genero)

saveCliente :: Cliente -> IO()
saveCliente c = do
    out <- openFile "cliente.db" AppendMode
    hPutStrLn out (show c)
    hClose out

deleteCliente :: IO()
deleteCliente = do
    inp <- openFile "cliente.db" ReadMode
    openCliente inp
    print("ID do cliente a ser removido:")
    deleteidstr <- getLine
    let deleteid = (read deleteidstr :: Int)
    hClose inp
    renameFile "cliente.db" "oldcliente.db"
    inp <- openFile "oldcliente.db" ReadMode
    out <- openFile "cliente.db" AppendMode
    deleteClienteFromID deleteid inp out
    hClose inp
    hClose out
    removeFile "oldcliente.db"

deleteClienteFromID :: Int -> Handle -> Handle -> IO()
deleteClienteFromID id inp out =
    do inpeof <- hIsEOF inp
       if inpeof
            then return()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Cliente)
                if (clienteGetCod select)==id
                    then print("ta-dah")
                    else hPutStrLn out inpString
                deleteClienteFromID id inp out

openCliente :: Handle -> IO()
openCliente h = 
    do heof <- hIsEOF h
       if heof
            then return()
            else do
                inpString <- hGetLine h
                print(inpString)
                openCliente h

main :: IO ()
main = do
    print ("Escolha uma opcao:")
    print ("1 - adicionar servico")
    print ("2 - remover servico")
    print ("3 - alterar servico")
    print ("4 - adicionar cliente")
    print ("5 - remover cliente")
    print ("6 - alterar cliente")
    inpString <- getLine
    let inp = (read inpString :: Int)
    if (inp==1)
        then newServico
        else if (inp==2)
            then deleteServico
            else if (inp==3)
                then changeServico
                else if (inp==4)
                    then newCliente
                    else if (inp==5)
                        then deleteCliente 
                        else do print("nao foi")