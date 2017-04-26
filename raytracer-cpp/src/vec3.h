#pragma once

#include <math.h>
#include <stdlib.h>

#define RAND(a, b) ((a) + (((b) - (a)) * ((rand() % 10000) / 10000.0)))

class Vec3 {
    public:
        float x, y, z;

        Vec3() {
            this->x = 0.0;
            this->y = 0.0;
            this->z = 0.0;
        };

        Vec3(float x, float y, float z) {
            this->x = x;
            this->y = y;
            this->z = z;
        };

        static float length(const Vec3 &v);
        static Vec3 normalize(const Vec3 &v);
        static Vec3 reflect(const Vec3 &v, const Vec3 &n);
        static float dot(const Vec3 &v1, const Vec3 &v2);
        static Vec3 cross(const Vec3 &v1, const Vec3 &v2);
        static Vec3 random_in_unit_disk();
        static Vec3 random_in_unit_sphere();
        static Vec3 clamp(const Vec3 &v, float min, float max);
        static Vec3 rotate_x(const Vec3 &v, float cos_theta, float sin_theta);
        static Vec3 rotate_y(const Vec3 &v, float cos_theta, float sin_theta);
        static Vec3 rotate_z(const Vec3 &v, float cos_theta, float sin_theta);
};

Vec3 operator+(const Vec3 &u, const Vec3 &v);
Vec3 operator-(const Vec3 &u, const Vec3 &v);
Vec3 operator*(float s, const Vec3 &v);

class Vec2 {
    public:
        float x, y;

        Vec2() {
            this->x = 0.0;
            this->y = 0.0;
        };

        Vec2(float x, float y) {
            this->x = x;
            this->y = y;
        };
};
