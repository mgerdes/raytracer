#include "material.h"

ScatterResult LambertianMaterial::scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord) {
    ScatterResult result;

    Vec3 target = normal + Vec3::random_in_unit_sphere();
    result.ray = Ray(position, Vec3::normalize(target)); 
    result.color = albedo->value(texture_coord, position);
    result.did_scatter = true;

    return result;
}

Vec3 LambertianMaterial::emitted(const Vec3 &p) {
    return Vec3(0.0, 0.0, 0.0);
}

ScatterResult MetalMaterial::scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord) {
    ScatterResult result;

    Vec3 target = Vec3::reflect(ray.direction, normal);
    result.ray = Ray(position, Vec3::normalize(target)); 
    result.color = albedo->value(texture_coord, position);
    result.did_scatter = true;

    return result;
}

Vec3 MetalMaterial::emitted(const Vec3 &p) {
    return Vec3(0.0, 0.0, 0.0);
}

ScatterResult DiffuseLightMaterial::scatter(const Ray &ray, const Vec3 &position, const Vec3 &normal, const Vec2 &texture_coord) {
    ScatterResult result;
    result.did_scatter = false;
    return result;
}

Vec3 DiffuseLightMaterial::emitted(const Vec3 &p) {
    return albedo->value(Vec2(0.0, 0.0), p);
}
