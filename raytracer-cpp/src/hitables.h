#pragma once

#include <vector>
#include <cfloat>

#include "hit_record.h"
#include "vec3.h"
#include "ray.h"

class Hitable {
    public:
        Material *material;

        virtual HitRecord intersect(const Ray &ray) = 0;
};

class Sphere : public Hitable {
    public:
        Vec3 position;
        float radius;

        Sphere(const Vec3 &position, float radius) {
            this->position = position;
            this->radius = radius;
        }

        HitRecord intersect(const Ray &ray);
};

class XYRect : public Hitable {
    public:
        Vec3 min, max;

        XYRect() { };

        XYRect(const Vec3 &min, const Vec3 &max) {
            this->min = min;
            this->max = max;
        };

        HitRecord intersect(const Ray &ray);
};

class TransformedHitable : public Hitable {
    private:
        float cos_theta_x, sin_theta_x;
        float cos_theta_y, sin_theta_y;
        float cos_theta_z, sin_theta_z;

    public:
        Hitable *hitable;
        Vec3 translation, rotation, scale;

        TransformedHitable() { 
            this->hitable = nullptr; 
        };

        TransformedHitable(Hitable *hitable, const Vec3 &translation, const Vec3 &rotation, const Vec3 &scale) {
            this->hitable = hitable;
            this->translation = translation;
            this->rotation = rotation;
            this->scale = scale;

            cos_theta_x = cos(rotation.x);
            sin_theta_x = sin(rotation.x);

            cos_theta_y = cos(rotation.y);
            sin_theta_y = sin(rotation.y);

            cos_theta_z = cos(rotation.z);
            sin_theta_z = sin(rotation.z);
        };

        HitRecord intersect(const Ray &ray);
};

class HitableList : public Hitable {
    public:
        std::vector<Hitable*> hitables;

        HitRecord intersect(const Ray &ray);
};

class Box : public Hitable {
    public: 
        Vec3 min, max;

        Box(const Vec3 &min, const Vec3 &max) {
            this->min = min;
            this->max = max;
        };

        HitRecord intersect(const Ray &ray);
};
