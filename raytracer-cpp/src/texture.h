#pragma once

#include <math.h>

#include "vec3.h"
#include "stb_image.h"

class Texture {
    public:
        virtual Vec3 value(const Vec2 &t, const Vec3 &p) = 0;
};

class ConstantTexture : public Texture {
    public:
        Vec3 color;

        ConstantTexture(const Vec3 &color) {
            this->color = color;
        };

        Vec3 value(const Vec2 &t, const Vec3 &p);
};

class CheckeredTexture : public Texture {
    public:
        Texture *texture0, *texture1;

        CheckeredTexture(Texture *texture0, Texture *texture1) {
            this->texture0 = texture0;
            this->texture1 = texture1;
        };

        Vec3 value(const Vec2 &t, const Vec3 &p);
};

class ImageTexture : public Texture {
    public:
        int width, height;
        unsigned char *data;

        ImageTexture(const char *image_file_name) {
            int n;
            data = stbi_load(image_file_name, &width, &height, &n, 3);
        };

        Vec3 value(const Vec2 &t, const Vec3 &p);
};
