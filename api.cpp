#include <iostream>
#include <vector>
#include <fstream>

bool drawing;
typedef char byte;

union buffer{
    int integer;
    char chars[4];
};

void gmbARUMAITO(buffer &size){
    byte aux = size.chars[3];
    size.chars[3] = size.chars[0];
    size.chars[0] = aux;
    aux = size.chars[2];
    size.chars[2] = size.chars[1];
    size.chars[1] = aux;
}

void allData(std::string image_path, std::string label_path){
    std::ifstream image;
    std::ifstream label;
    image.open(image_path, std::ifstream::in);
    label.open(label_path, std::ifstream::in);
    buffer size;
    label.read(size.chars, 4);
    gmbARUMAITO(size);
    label.read(size.chars, 4);
    gmbARUMAITO(size);
    std::vector < byte > labels;
    byte l;
    for(register int i = 0; i < size.integer; i++){
        label.read(&l, 1);
        labels.push_back(l);
    }
    buffer rows, colunms;
    image.read(size.chars, 4);image.read(size.chars, 4);gmbARUMAITO(size);
    image.read(rows.chars, 4);gmbARUMAITO(rows);
    image.read(colunms.chars, 4);gmbARUMAITO(colunms);
    for(int a = 0; a < size.integer; a++){
        std::cout << ((int)labels[a]);
        for(register int i = 0; i < rows.integer; i++){
            for(register int j = 0; j < colunms.integer; j++){
                image.read(&l, 1);
                std::cout << " " << (((int)((unsigned char)l))/255.0);
            }
        }
        std::cout << std::endl;
    }
    return;
}

int main(int argc, char *argv[]){
    allData("src/Training/train-images-idx3-ubyte", "src/Training/train-labels-idx1-ubyte");
    return 0;
}