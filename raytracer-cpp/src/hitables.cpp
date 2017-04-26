#include "hitables.h"

HitRecord Sphere::intersect(const Ray &ray) {
    HitRecord result;

    Vec3 ro = ray.origin;
    Vec3 rd = ray.direction;
    Vec3 so = this->position;

    float a = Vec3::dot(rd, rd);
    float b = 2.0 * (Vec3::dot(rd, ro - so));
    float c = Vec3::dot(ro - so, ro - so) - this->radius * this->radius;
    float det = b * b - 4 * a * c;

    if (det < 0.0) {
        result.did_hit = false;
        return result;
    }

    float t1 = (-b + sqrt(det)) / (2.0 * a);
    float t2 = (-b - sqrt(det)) / (2.0 * a);
    float t = t1 < t2 ? t1 : t2;

    if (t < 0.0) {
        result.did_hit = false;
        return result;
    }

    result.did_hit = true;
    result.t = t;
    result.position = ray.point_at_time(t);
    result.normal = Vec3::normalize(result.position - so);
    result.material = this->material;

    {
        Vec3 texture_position = (1.0 / this->radius) * result.normal;
        float phi = atan2(texture_position.z, texture_position.x);
        float theta = asin(texture_position.y);
        float u = 1.0 - (phi + M_PI) / (2.0 * M_PI);
        float v = (theta + (0.5 * M_PI)) / M_PI;
        result.texture_coord = Vec2(u, v);
    }

    return result;
}

HitRecord XYRect::intersect(const Ray &ray) {
    HitRecord result;

    float t = ((max.z - ray.origin.z) / ray.direction.z) - 0.0001;
    Vec3 position = ray.point_at_time(t);

    if (t < 0.0 || position.x < min.x || position.x > max.x || position.y < min.y || position.y > max.y) {
        result.did_hit = false;
        return result;
    }

    result.did_hit = true;
    result.t = t;
    result.position = position;
    result.normal = Vec3(0.0, 0.0, 1.0);
    result.material = material;
    result.texture_coord = Vec2((position.x - min.x) / (max.x - min.x), (position.y - min.y) / (max.y - min.y));
    return result;
}

HitRecord Box::intersect(const Ray &ray) {
    HitRecord result;

    float t0x = (min.x - ray.origin.x) / ray.direction.x;
    float t1x = (max.x - ray.origin.x) / ray.direction.x;

    if (t0x > t1x) {
        float temp = t0x;
        t0x = t1x;
        t1x = temp;
    }

    float t0y = (min.y - ray.origin.y) / ray.direction.y;
    float t1y = (max.y - ray.origin.y) / ray.direction.y;

    if (t0y > t1y) {
        float temp = t0y;
        t0y = t1y;
        t1y = temp;
    }

    float t0z = (min.z - ray.origin.z) / ray.direction.z;
    float t1z = (max.z - ray.origin.z) / ray.direction.z;

    if (t0z > t1z) {
        float temp = t0z;
        t0z = t1z;
        t1z = temp;
    }

    float t0 = t0x;
    float t1 = t1x;

    int t0_index = 0;
    int t1_index = 0;

    if (t0y > t0) {
        t0 = t0y;
        t0_index = 1;
    }
    if (t1y < t1) {
        t1 = t1y;
        t1_index = 1;
    }

    if (t0z > t0) {
        t0 = t0z;
        t0_index = 2;
    }
    if (t1z < t1) {
        t1 = t1z;
        t1_index = 2;
    }

    if (t0 < 0.0 || t0 > t1) {
        result.did_hit = false;
        return result;
    }

    if (t0_index == 0) {
        result.normal = Vec3(1.0, 0.0, 0.0);
    }
    else if (t0_index == 1) {
        result.normal = Vec3(0.0, 1.0, 0.0);
    }
    else if (t0_index == 2) {
        result.normal = Vec3(0.0, 0.0, 1.0);
    }

    if (Vec3::dot(result.normal, ray.direction) > 0.0) {
        result.normal = -1.0 * result.normal;
    }
    
    result.did_hit = true;
    result.t = t0 - 0.0001;
    result.position = ray.point_at_time(t0 - 0.0001);
    result.material = this->material;
    result.texture_coord = Vec2(0.0, 0.0);

    return result;
}

HitRecord TransformedHitable::intersect(const Ray &ray) {
    if (!hitable) {
        HitRecord record;
        record.did_hit = false;
        return record;
    }

    Ray transformed_ray(ray);

    transformed_ray.origin = transformed_ray.origin - translation;

    transformed_ray.origin = Vec3::rotate_x(transformed_ray.origin, cos_theta_x, -sin_theta_x);
    transformed_ray.direction = Vec3::rotate_x(transformed_ray.direction, cos_theta_x, -sin_theta_x);

    transformed_ray.origin = Vec3::rotate_y(transformed_ray.origin, cos_theta_y, -sin_theta_y);
    transformed_ray.direction = Vec3::rotate_y(transformed_ray.direction, cos_theta_y, -sin_theta_y);

    transformed_ray.origin = Vec3::rotate_z(transformed_ray.origin, cos_theta_z, -sin_theta_z);
    transformed_ray.direction = Vec3::rotate_z(transformed_ray.direction, cos_theta_z, -sin_theta_z);

    HitRecord record = hitable->intersect(transformed_ray);

    if (record.did_hit) {
        record.position = Vec3::rotate_z(record.position, cos_theta_z, sin_theta_z);
        record.position = Vec3::rotate_y(record.position, cos_theta_y, sin_theta_y);
        record.position = Vec3::rotate_x(record.position, cos_theta_x, sin_theta_x);
        record.position = record.position + translation;

        record.normal = Vec3::rotate_z(record.normal, cos_theta_z, sin_theta_z);
        record.normal = Vec3::rotate_y(record.normal, cos_theta_y, sin_theta_y);
        record.normal = Vec3::rotate_x(record.normal, cos_theta_x, sin_theta_x);
    }

    return record;
}

HitRecord HitableList::intersect(const Ray &ray) {
    HitRecord result;
    result.did_hit = false;
    result.t = FLT_MAX;

    for (int i = 0; i < hitables.size(); i++) {
        HitRecord temp_result = hitables[i]->intersect(ray);

        if (temp_result.did_hit && temp_result.t < result.t) {
            result = temp_result;
        }
    }

    return result;
}
