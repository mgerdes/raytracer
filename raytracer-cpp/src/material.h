#pragma once

#include "vec3.h"
#include "texture.h"
#include "ray.h"

struct ScatterResult {
    bool did_scatter;
    Ray ray;
    Vec3 color;
};

class Material {
    public:
        virtual ScatterResult scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord) = 0; 
        virtual Vec3 emitted(const Vec3 &p) = 0;
};

class LambertianMaterial : public Material {
    public:
        Texture *albedo; 

        LambertianMaterial(Texture *albedo) { 
            this->albedo = albedo;
        };

        ScatterResult scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord);
        Vec3 emitted(const Vec3 &p);
};

class MetalMaterial : public Material {
    public:
        Texture *albedo;

        MetalMaterial(Texture *albedo) {
            this->albedo = albedo;
        };

        ScatterResult scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord);
        Vec3 emitted(const Vec3 &p);
};

class DiffuseLightMaterial : public Material {
    public:
        Texture *albedo;

        DiffuseLightMaterial(Texture *albedo) {
            this->albedo = albedo;
        };

        ScatterResult scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord);
        Vec3 emitted(const Vec3 &p);
};
