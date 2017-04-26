#include "texture.h"

Vec3 ConstantTexture::value(const Vec2 &t, const Vec3 &p) {
    return color;   
}

Vec3 CheckeredTexture::value(const Vec2 &t, const Vec3 &p) {
    float sines = sin(10 * p.x) * sin(10 * p.y) * sin(10 * p.z); 

    if (sines < 0) {
        return texture0->value(t, p);
    }
    else {
        return texture1->value(t, p);
    }
}

Vec3 ImageTexture::value(const Vec2 &t, const Vec3 &p) {
    int x = width * t.x;
    int y = height * t.y;

    unsigned char r = data[3 * (x * height + y) + 0]; 
    unsigned char g = data[3 * (x * height + y) + 1]; 
    unsigned char b = data[3 * (x * height + y) + 2]; 

    return Vec3(r / 255.0, g / 255.0, b / 255.0);
}
